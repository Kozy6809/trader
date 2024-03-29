package trader

import java.time.temporal.ChronoUnit

object SlidingStrategy extends Strategy {
  object  Status extends Enumeration {
    val MAY_ENTER, ENTRY, ENTERED, IN_THERE, MAY_CHANGE, OUT_THERE = Value
  }
  private var status = Status.OUT_THERE
  private var holding = Judgement.STAY
  private var positionPrice: Price = _
  private var leastProfittablePrice = 0.0
  private var nearPosition = 0.0 // ポジション価格に近い外側の価格(デフォルトで4tick)
  private var direction = 0 // 予想される値動き。上昇/下降/停滞
  private var positionDirection = 0
  private var prevDirection = 0
  private var entryPrice: Price = _
  private var runSlide = 1
  private var counterMovement = 0

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

  private def m(ix: Int) = SlidingWindow.slides(ix).metrics.head
  private def s(ix: Int) = SlidingWindow.slides(ix).data.head.askPrice

  override def add(p: Price): Judgement.Value = {
    if (SlidingWindow.newSlide && SlidingWindow.slides.take(3).size >= 3) {
      val s0 = s(0)
      val s1 = s(1)
      val s2 = s(2)
      if (s0 >= s1 && s1 >= s2 || s0 <= s1 && s1 <= s2) runSlide += 1
      else runSlide = 1
      prevDirection = direction
      direction = (s0 - s1).sign.toInt
      if (direction != positionDirection) counterMovement += 1
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
        positionPrice = p
        leastProfittablePrice = positionPrice.askPrice + 10.0 * positionDirection
        nearPosition = positionPrice.askPrice + 20.0 * positionDirection
        println(s"$holding positioned at ${positionPrice.askPrice}")
        holding
      case Status.IN_THERE =>
        Judgement.STAY
      case Status.MAY_CHANGE =>
        status = Status.OUT_THERE
        println(s"$direction $prevDirection settled at ${p.askPrice}")
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
  private def isInner(price: Double, ref: Double): Boolean = (price - ref).sign * positionDirection <= 0
  private def laps(from: Price, to: Price): Long = from.time.until(to.time, ChronoUnit.SECONDS)

  private def analyze(p: Price): Status.Value = {

    def isMayEnter(p: Price): Boolean = {
      if (SlidingWindow.newSlide && runSlide >= 2) {
        val m0 = m(0)
        val m1 = m(1)
        val s0 = s(0)
        val s1 = s(1)
        val di5 = s0 - m0.m5
        val dm5 = m0.m5 - m1.m5
        val stg = Metrics.metrics.head.stage
        if (dm5.abs > 1 && (s0 - s1).sign == di5.sign && di5.sign == dm5.sign
          && (di5 + dm5).abs > 15.0) {
          if  ((di5 < 0 && stg != 1) || (di5 > 0 && stg != 4)) {
            positionDirection = di5.sign.toInt
            direction = positionDirection
            true
          } else false
        } else false
      } else false
    }

    def isMayChange(p: Price): Boolean = {
      if (SlidingWindow.newSlide) {
        if (counterMovement > 1) {
          counterMovement = 0
          true
        } else false
      } else false
    }

    status match {
      case Status.OUT_THERE =>
        if (isMayEnter(p)) Status.MAY_ENTER else Status.OUT_THERE
      case Status.MAY_ENTER =>
        Status.ENTRY
      case Status.ENTRY =>
        //        if (p.askPrice != currentHaken.p.askPrice && isInner(p.askPrice, currentHaken.p.askPrice)) Status.ENTERED
        //        else Status.ENTRY
        Status.ENTERED
      case Status.ENTERED =>
        Status.IN_THERE
      case Status.IN_THERE =>
        if (isMayChange(p)) Status.MAY_CHANGE else Status.IN_THERE
      case Status.MAY_CHANGE =>
        Status.OUT_THERE
    }

  }
}
