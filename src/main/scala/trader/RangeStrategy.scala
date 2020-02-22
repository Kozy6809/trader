package trader

import java.time.LocalTime

/**
  * 直近の価格レンジが5ティック以上の時、現在値が下限+1ティック以下なら買い、上限-1ティック以上なら売り
  * 単純な現在レンジの上下端で売買する戦略では、以下のようにロスカットしない時が一番収益が良かった
  * 10:-565 15:-585 20:-205 25:-55 30:+200 35:+155 none:+920
  * 値動きレンジが変わる時に、それまでのレンジの建玉と逆方向に変わる時は建玉を決済する
  * expandModeがfalseの時はボックスレンジ戦略で上下の値でポジションを取る
  * expandModeの時はそのトレンドに合致したポジションを取る
  * クロージングでは決済のみを行う
  */
object RangeStrategy extends Strategy {
  private var prevRange = (0.0, 0.0)
  private var currentRange = (0.0, 0.0)
  private var prevMetrics = (0.0, 0.0, 0.0, 0.0, 0.0)
  private var currentMetrics = (0.0, 0.0, 0.0, 0.0, 0.0)
  private var holding = Judgement.STAY
  private var holdingPrice = 0.0
  private var expandMode = false
  private var boxMode = false
  private var transition = false // モードが切り替わった時だけtrue
  private var madiffer = false // metrics.m1280とm2560の値が異なり始めたらtrue
  private var trend = 0.0
  private var prevTrend = 0.0
  private var prevPrevTrend = 0.0
  private var prevmsg = ""
  private var status = State.RANGECHANGED

  object State extends Enumeration {
    val CALM, BOX, RANGEMAYBECHANGING, RANGECHANGED, RANGEESTIMATION, STEADY, STEEP = Value
  }

  def isMadiffer: Boolean = {
    if (!madiffer) {
      val m = Metrics.metrics.head
      if (m.m20 != m.m40) madiffer = true
    }
    madiffer
  }

  def isBox: Boolean = {
    val m = Metrics.metrics.head
    if ((m.m5 - m.m20).abs >= 12.5 || (m.m5 - m.m20).abs >= 12.5) {
      transition = boxMode
      boxMode = false
    }
    if ((m.m5 - m.m20).abs <= 10.0 && (m.m5 - m.m40).abs <= 10.0) {
      val rate = TechAnal.maRate
      if (rate._2.abs <= 0.2 && rate._3.abs <= 0.2) {
        transition = !boxMode
        boxMode = true
      }
    }
    boxMode
  }

  def isExpand: Boolean = { // 値が一定の方向に動き出しているかどうか。閾値はシュミットトリガになっている
    val diff = currentRange._2 - currentRange._1
    val prevDiff = prevRange._2 - prevRange._1
    if (diff - prevDiff >= 15.0) expandMode = true
    else if (diff < 50.0) expandMode = false
    else if (diff >= 60.0) expandMode = true

    expandMode
  }

  def isUpeak: Boolean = TechAnal.upeaks.head._2 == TechAnal.data.length - 1
  def isLpeak: Boolean = TechAnal.lpeaks.head._2 == TechAnal.data.length - 1

  def isSliding(p: Price): Boolean = {
    if (currentRange._2 > prevRange._2 && p.price >= currentRange._2 - 10.0) true
    else if (currentRange._1 < prevRange._1 && p.price <= currentRange._1 + 10.0) true
    else false
  }

  def holdSell(p: Price): Judgement.Value = {
    if (holding != Judgement.SELL) holdingPrice = p.price
    holding = Judgement.SELL
    holding
  }

  def holdBuy(p: Price): Judgement.Value = {
    if (holding != Judgement.BUY) holdingPrice = p.price
    holding = Judgement.BUY
    holding
  }

  def settleSell(): Judgement.Value = {
    holding = Judgement.STAY
    Judgement.SETTLE_SELL
  }

  def settleBuy(): Judgement.Value = {
    holding = Judgement.STAY
    Judgement.SETTLE_BUY
  }

  def newmsg(p: Price, msg: String): Unit = {
    if (msg != prevmsg) {
      StockLogger.bsMessage(s"${p.time.toLocalTime} ${p.price} $msg")
      prevmsg = msg
    }
  }

  override def add(p: Price): Judgement.Value = {
    //    if (p.time.toLocalTime.compareTo(LocalTime.of(23,0)) < 0) return Judgement.STAY
    var decision = Judgement.STAY
    if (TechAnal.isNewRange) {
      prevRange = currentRange
      currentRange = TechAnal.currentRange
      if (prevRange._1 == 0.0) prevRange = currentRange
      prevPrevTrend = prevTrend
      prevTrend = trend
      trend = TechAnal.rangeTrend
      //      println(f"${p.time} $prevTrend%.1f $trend%.1f")
    }
    if (Technical.isClosing(p.time.toLocalTime)) {
      if (holding == Judgement.BUY) decision = settleSell()
      else if (holding == Judgement.SELL) decision = settleBuy()
    } else if (isMadiffer) {
      val width = currentRange._2 - currentRange._1
      val upperBound = currentRange._2 - (width - 10) / 2
      val lowerBound = currentRange._1 + (width - 10) / 2
      // rule set
      if (!isBox) {
        newmsg(p, "not box")
//        val rate = TechAnal.maRate
//        if (transition) {
//          if (rate._1 > 0) decision = holdBuy(p)
//          else if (rate._1 < 0) decision = holdSell(p)
//        } else if (rate._2 > 0 && rate._3 > 0) {
//          if (isLpeak) decision = holdBuy(p)
//          else if (isUpeak) decision = holdSell(p)
//        }
//        else if (rate._2 < 0 && rate._3 < 0) {
//          if (isLpeak) decision = holdBuy(p)
//          else if (isUpeak) decision = holdSell(p)
//        }
//        else if (rate._2 < 0 && rate._3 > 0) {
//          if (isUpeak || rate._1 < 0) decision = holdSell(p)
//        } else if (rate._2 > 0 && rate._3 < 0) {
//          if (isLpeak || rate._1 > 0) decision = holdBuy(p)
//        }
      } else {
        newmsg(p, s"box $prevRange $currentRange")
        // ポジションがレンジの切り替わりのために逆になったら決済する
        if (holding == Judgement.BUY && holdingPrice >= currentRange._2 - (width / 2)) decision = settleSell()
        else if (holding == Judgement.SELL && holdingPrice <= currentRange._1 + (width / 2)) decision = settleBuy()

        // ボックスレンジでポジションを取る
        else if (width >= 25.0 && p.price >= upperBound) decision = holdSell(p)
        else if (width >= 25.0 && p.price <= lowerBound) decision = holdBuy(p)
        // ボックスレンジで決済する
        else if (holding == Judgement.BUY && p.price > holdingPrice + 10.0) decision = settleSell()
        else if (holding == Judgement.SELL && p.price < holdingPrice - 10.0) decision = settleBuy()
        else decision = Judgement.STAY
      }
    } else decision = Judgement.STAY


    if (TechAnal.slides.length >= 10) decision else Judgement.STAY
  }

  override def reset(): Unit = {
    prevRange = (0.0, 0.0)
    currentRange = (0.0, 0.0)
    holding = Judgement.STAY
    holdingPrice = 0.0
  }
}
