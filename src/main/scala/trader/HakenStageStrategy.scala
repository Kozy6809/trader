package trader

import java.time.temporal.ChronoUnit

object HakenStageStrategy extends Strategy {
  object  Status extends Enumeration {
    val MAY_ENTER, ENTRY, ENTERED, IN_THERE, MAY_CHANGE, OUT_THERE = Value
  }
  private var status = Status.OUT_THERE
  private var holding = Judgement.STAY
  private var currentHaken: Haken = _
  private var positionPrice: Price = _
  private var direction = 0 // 予想される値動き。上昇/下降/停滞
  private var prevDirection = 0
  private var entryPrice: Price = _

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
//        println(holding + " positioned at " + positionPrice.askPrice)
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

  private def innerPrice(p: Price, offset: Double): Double = p.askPrice - offset * direction
  private def isInner(price: Double, ref: Double): Boolean = (price - ref).sign * direction <= 0
  private def laps(from: Price, to: Price): Long = from.time.until(to.time, ChronoUnit.SECONDS)

  private def analyze(p: Price): Status.Value = {

    def isMayEnter(p: Price): Boolean = {
      val h = Haken
      val m = Metrics.metrics.head
      if (h.newHaken) {
        m.maRange >= 5.0 && (
        (direction > 0 && (m.stage == 6 || m.stage == 1)) ||
          (direction < 0 && (m.stage == 3 || m.stage == 4))
          )
      } else false
    }

    def isMayChange(p: Price): Boolean = {
        (Haken.newHaken && direction != prevDirection) ||
          isInner(p.askPrice, innerPrice(currentHaken.p, Settings.hakenDeclineThreshold))
    }

    status match {
      case Status.OUT_THERE =>
        if (isMayEnter(p)) Status.MAY_ENTER else Status.OUT_THERE
      case Status.MAY_ENTER =>
        Status.ENTRY
      case Status.ENTRY =>
        if (p.askPrice != currentHaken.p.askPrice && isInner(p.askPrice, currentHaken.p.askPrice)) Status.ENTERED
        else Status.ENTRY
      case Status.ENTERED =>
        Status.IN_THERE
      case Status.IN_THERE =>
        if (isMayChange(p)) Status.MAY_CHANGE else Status.IN_THERE
      case Status.MAY_CHANGE =>
        Status.OUT_THERE
    }

  }
}
