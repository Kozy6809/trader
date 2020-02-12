package trader

object TrendStrategy extends Strategy {
  object  Status extends Enumeration {
    val MAY_ENTER, IN_THERE, MAY_CHANGE, OUT_THERE = Value
  }
  private var status = Status.OUT_THERE
  private var holding = Judgement.STAY
  private var positionAskPrice = 0.0
  private var direction = 0 // 予想される値動き。上昇/下降/停滞
  private var pole = 0.0

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
    analyze(p) match {
      case Status.MAY_ENTER =>
        status = Status.IN_THERE
        positionAskPrice = p.askPrice
        pole = p.askPrice
        holding = direction2action()
        println(holding + " positioned at " + positionAskPrice)
        holding
      case Status.MAY_CHANGE =>
        status = Status.OUT_THERE
        println("settled at " + p.askPrice)
        val r = settle(holding)
        pole = 0.0
        holding = Judgement.STAY
        r
      case _ => Judgement.STAY
    }
  }

  override def reset(): Unit =  {
    status = Status.OUT_THERE
    holding = Judgement.STAY
    positionAskPrice = 0.0
  }

  private def analyze(p: Price): Status.Value = {
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
      if (diffma._1.abs > 1.0 && diffma._2.abs > 1.0 &&
        (p.askPrice - m.m320).abs > 5.0 && (m.m320 - m.m640).abs > 2.5 &&
        (m.m640 - m.m1280).abs > 1.0) {
        val sumsgn = diffma._1.signum + diffma._2.signum +
          (p.askPrice - m.m320).signum + (m.m320 - m.m640).signum + (m.m640 - m.m1280).signum
        if (sumsgn.abs == 5) {
          direction = sumsgn.signum
          val r = SlidingWindow.calcRange()
          val e = if (direction > 0) r._2 else r._1
          e == p.askPrice
        } else false
      } else false
    }

    /*
    終了条件:
    askPriceがm640を逆方向に割り込む
    sgn(askPrice - m640) == -direction
     */
    def isMayChange(p: Price): Boolean = {
      if (pole == 0.0 ||
        (direction > 0 && pole < p.askPrice) ||
        (direction < 0 && pole > p.askPrice)) {
        pole = p.askPrice
      }
      val d = p.askPrice - positionAskPrice
      val d320 = p.askPrice - TechAnal.metrics.head.m320
      val r = (d.abs >= 10 && d.signum == direction) ||
        (d.abs >= 5.0 && d.signum == -direction && d320.signum == -direction)
//      val r = (pole - p.askPrice).abs >= 15.0 && (pole - p.askPrice).signum == direction
      if (r) println(pole +" "+ p.askPrice)
      r
//      (p.askPrice - TechAnal.metrics.head.m640).signum == -direction
    }

    status match {
      case Status.OUT_THERE =>
        if (isMayEnter(p)) Status.MAY_ENTER else Status.OUT_THERE
      case Status.MAY_ENTER =>
        Status.IN_THERE
      case Status.IN_THERE =>
        if (isMayChange(p)) Status.MAY_CHANGE else Status.IN_THERE
      case Status.MAY_CHANGE =>
        Status.OUT_THERE
    }

  }
}
