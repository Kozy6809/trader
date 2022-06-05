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
        println(s"$holding positioned at $positionAskPrice")
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
      val m = Metrics.metrics.head
      if (diffma._1.abs > 0.0 && diffma._2.abs > 0.0 &&
        (p.askPrice - m.m5).abs > 5.0 && (m.m5 - m.m10).abs > 0.0 &&
        (m.m10 - m.m20).abs > 0.0) {
        val sumsgn = diffma._1.sign + diffma._2.sign +
          (p.askPrice - m.m5).sign + (m.m5 - m.m10).sign + (m.m10 - m.m20).sign
        if (sumsgn.abs == 5.0) {
          direction = sumsgn.sign.toInt
          val r = SlidingWindow.calcRange()
          val e = if (direction > 0) r._2 else r._1
          r._2 - r._1 >= 30.0 && e == p.askPrice
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
      val m = Metrics.metrics
      val diffma = TechAnal.maDiff()
      val df320 = diffma._1
      val d = p.askPrice - positionAskPrice
      val d320 = p.askPrice - m.head.m5
      val d640 = p.askPrice - m.head.m10
      val d1280 = p.askPrice - m.head.m20
      val dpp = pole - positionAskPrice
      val dpask = p.askPrice - positionAskPrice
      val dp320 = pole - m.head.m5
      val drb = m.head.m20 - m.head.m40
      // scalping
//      val r = (d.abs >= 10 && d.signum == direction) ||
//        (d.abs >= 5.0 && d.signum == -direction && d320.signum == -direction)

//      val r = (pole - p.askPrice).abs >= 15.0 && (pole - p.askPrice).signum == direction
//      if (r) println(pole +" "+ p.askPrice)
//      r
      d1280.sign == -direction ||
        (!(drb.sign == direction && drb.abs >= 5.0) && (
      (dpp == 0.0 && d.abs >= 10.0 && d.sign == -direction && df320.abs < 1.0 && df320.sign == -direction) ||
        (dpp.abs == 5.0 && d.abs >= 5.0 && d.sign == -direction && d320.sign == -direction) ||
        (dpp.abs == 10.0 && d.sign == -direction && d320.sign == -direction) ||
        (dpp.abs >= 15.0 && dpask.abs <= dp320.abs * 0.5 && dpask.sign == direction)
         ||
      d640.sign == -direction))
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
