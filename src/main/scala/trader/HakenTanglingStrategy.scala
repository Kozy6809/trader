package trader

import java.time.temporal.ChronoUnit

object HakenTanglingStrategy extends Strategy {
  object  Status extends Enumeration {
    val MAY_ENTER, ENTRY, ENTERED, IN_THERE, MAY_CHANGE, OUT_THERE = Value
  }
  private var status = Status.OUT_THERE
  private var holding = Judgement.STAY
  private var currentHaken: Haken = _
  private var mostOuterHaken: Haken = _ // ポジションから最も離れたHaken
  private var positionPrice: Price = _
  private var leastProfittablePrice = 0.0
  private var nearPosition = 0.0 // ポジション価格に近い外側の価格(デフォルトで4tick)
  private var direction = 0 // 予想される値動き。上昇/下降/停滞
  private var positionDirection = 0
  private var positionCount = 0 // ポジション取得後のHaken到来回数
  private var prevDirection = 0
  private var entryPrice: Price = _
  private val rangeThreshold = Array(40.0, 35.0, 30.0, 25.0)

  private def direction2action(): Judgement.Value = {
    if (direction > 0) Judgement.BUY
    else if (direction < 0) Judgement.SELL
    else Judgement.STAY
  }

  private def settle(holding: Judgement.Value): Judgement.Value = {
    holding match {
      case Judgement.BUY => Judgement.SETTLE_SELL
      case Judgement.SELL => Judgement.SETTLE_BUY
      case _ => Judgement.STAY
    }
  }

  override def add(p: Price): Judgement.Value = {
    if (Haken.newHaken) {
      prevDirection = direction
      currentHaken = Haken.hakens.head
      direction = currentHaken.direction
      positionCount += 1
    }
    status = analyze(p)
    status match {
      case Status.MAY_ENTER =>
        holding = direction2action()
        entryPrice = p
        //        println(holding + " entry at " + entryPrice.askPrice)
        Judgement.STAY
      case Status.ENTRY =>
        Judgement.STAY
      case Status.ENTERED =>
        holding = direction2action()
        entryPrice = p
        mostOuterHaken = currentHaken
        positionPrice = p
        positionDirection = currentHaken.direction
        leastProfittablePrice = positionPrice.askPrice + 10.0 * positionDirection
        positionCount = 0
        nearPosition = positionPrice.askPrice + 20.0 * positionDirection
        println(holding + " positioned at " + positionPrice.askPrice)
        holding
      case Status.IN_THERE =>
        Judgement.STAY
      case Status.MAY_CHANGE =>
        status = Status.OUT_THERE
        println(direction +" "+ prevDirection +" settled at " + p.askPrice)
        val r = settle(holding)
        holding = Judgement.STAY
        r
      case _ => Judgement.STAY
    }
  }

  override def reset(): Unit =  {
    status = Status.OUT_THERE
    holding = Judgement.STAY
  }

  private def innerPrice(p: Price, offset: Double): Double = p.askPrice - offset * positionDirection
  private def isInner(price: Double, ref: Double): Boolean = (price - ref).signum * positionDirection <= 0
  private def laps(from: Price, to: Price): Long = from.time.until(to.time, ChronoUnit.SECONDS)

  private def analyze(p: Price): Status.Value = {

    def isMayEnter(p: Price): Boolean = {
      val h = Haken
      val m = Metrics.metrics.head
      if (h.varWorstDecline >= 225.0) false
      else if (h.newHaken) {
        val thrs = if (h.runlen > rangeThreshold.length) rangeThreshold(rangeThreshold.length - 1)
        else rangeThreshold(h.runlen - 1)
        m.rangeDistance(p.askPrice) >= thrs
      } else false
    }

    def isMayChange(p: Price): Boolean = {
      if (positionCount >= 4 && isInner(p.askPrice, leastProfittablePrice)) {
//        StockLogger.bsMessage(s"least profittable price ${p.askPrice} ${positionPrice.askPrice}")
        true
      } else if (isInner(p.askPrice, innerPrice(currentHaken.p, Settings.hakenDeclineThreshold))) {
//        StockLogger.bsMessage(s"too much decline ${p.askPrice} ${positionPrice.askPrice}")
        true
      } else {
          isInner(p.askPrice, currentHaken.metrics.m40) ||
            (isInner(p.askPrice, currentHaken.metrics.m20) && TechAnal.maDiff()._4.abs < 1.5)
      }
    }

    status match {
      case Status.OUT_THERE =>
        if (isMayEnter(p)) Status.ENTERED else Status.OUT_THERE
      case Status.MAY_ENTER =>
        Status.ENTRY
      case Status.ENTRY =>
        //        if (p.askPrice != currentHaken.p.askPrice && isInner(p.askPrice, currentHaken.p.askPrice)) Status.ENTERED
        //        else Status.ENTRY
        Status.ENTERED
      case Status.ENTERED =>
        Status.IN_THERE
      case Status.IN_THERE =>
        if (Haken.newHaken && isInner(mostOuterHaken.p.askPrice, currentHaken.p.askPrice)) mostOuterHaken = currentHaken
        if (isMayChange(p)) Status.MAY_CHANGE else Status.IN_THERE
      case Status.MAY_CHANGE =>
        Status.OUT_THERE
    }

  }
}
