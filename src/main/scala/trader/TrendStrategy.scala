package trader

class TrendStrategy extends Strategy {
  object  Status extends Enumeration {
    val MAY_ENTER, IN_THERE, MAY_CHANGE, OUT_THERE = Value
  }
  private var status = Status.OUT_THERE
  private var holding = Judgement.STAY
  private var positionAskPrice = 0.0
  private var direction = 0 // 予想される値動き。上昇/下降/停滞

  private def direction2action(): Judgement.Value = {
    if (direction > 0) Judgement.BUY
    else if (direction < 0) Judgement.SELL
    else Judgement.STAY
  }

  override def add(p: Price): Judgement.Value = {
    analyze(p) match {
      case Status.MAY_ENTER =>
        status = Status.MAY_ENTER
        positionAskPrice = p.askPrice
        holding = direction2action()
        holding

      case _ =>
    }
    Judgement.STAY
  }

  override def reset(): Unit =  {
    status = Status.OUT_THERE
    holding = Judgement.STAY
    positionAskPrice = 0.0
  }

  private def analyze(p: Price): Status.Value = {
    status match {
      case Status.OUT_THERE =>
        if (isMayEnter(p)) Status.MAY_ENTER
      case Status.MAY_ENTER =>
        Status.IN_THERE
      case Status.IN_THERE =>
      case Status.MAY_CHANGE =>
    }
    /*
    開始条件:
    abs(diffm320) > 0.3tick
    abs(diffm640) > 0.3tick
    delta(m320, m640) > 2tick
    delta(askPrice, m320) > 3tick
    sgn(diffma320) == sgn(diffma640) == sgn(diff(ask,ma320)) == sgn(diff(ma320, ma640))
    ・ボックスレンジから2ティックはみ出した
     */
    def isMayEnter(p: Price): Boolean = {
      val diffma = TechAnal.maDiff()
      val m = TechAnal.metrics.head
      if (diffma._1.abs > 1.5 && diffma._2.abs > 1.5 &&
        (m.m320 - m.m640).abs > 10.0 && (p.askPrice - m.m320).abs > 15.0) {
        val sumsgn = diffma._1.signum + diffma._2.signum + (p.askPrice - m.m320).signum + (m.m320 - m.m640).signum
        if (sumsgn.abs == 4) {
          direction = sumsgn.signum
          true
        } else false
      } else false
    }
    Status.OUT_THERE
  }
}
