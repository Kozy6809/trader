package trader

object TrendDetectStrategy extends Strategy {
  private val m = Metrics.metrics
  private var lowVolatility = true // SlidingWindowの直近10個の値幅が20円以下の時true
  private var buysell = Judgement.STAY
  private var prevDecision = Judgement.STAY
  var atPeakDetectedPrice = 0.0
  var prevPrice = 0.0

  def reset(): Unit = {
    buysell = Judgement.STAY
    atPeakDetectedPrice = 0.0
    prevPrice = 0.0
  }

  override def add(p: Price): Judgement.Value = {
    var decision = Judgement.STAY

    if (lowVolatility != checkLowVolatility) {
      lowVolatility = checkLowVolatility
//      println(p.time + " volatility changed. invert current trading mode at price " + p.price)
    }

    if (prevDecision == Judgement.BUY && p.price <= prevPrice - 15.0) decision = Judgement.SETTLE_SELL
    else if (prevDecision == Judgement.SELL && p.price >= prevPrice + 15.0) decision = Judgement.SETTLE_BUY
    else {
      val j = TechAnal.envelopeExtention(p.time)
      buysell match {
        case Judgement.STAY =>
          if (!j._1.isNaN && m.head.m5 > j._1 && Math.abs(p.price - prevPrice) > 10) {
            println(p.time + " price " + p.price + " exceeds. Let's buy!")
            decision = Judgement.BUY
            buysell = Judgement.BUY
            prevPrice = p.price
          } else if (!j._2.isNaN && m.head.m5 < j._2 && Math.abs(p.price - prevPrice) > 10) {
            println(p.time + " price " + p.price + " decline. Do sell!")
            decision = Judgement.SELL
            buysell = Judgement.SELL
            prevPrice = p.price
        }
        case Judgement.SELL =>
          if (!j._1.isNaN && m.head.m5 > j._1 && Math.abs(p.price - prevPrice) > 10) {
            if (!lowVolatility) {
              println(p.time + " price " + p.price + " exceeds. Let's buy!")
              decision = Judgement.BUY
            } else {
              println(p.time + " price " + p.price + " exceeds. Let's sell for lowV!")
              decision = Judgement.SELL
            }
            buysell = Judgement.BUY
            prevPrice = p.price
          }
        case Judgement.BUY =>
          if (!j._2.isNaN && m.head.m5 < j._2 && Math.abs(p.price - prevPrice) > 10) {
            if (!lowVolatility) {
              println(p.time + " price " + p.price + " decline. Do sell!")
              decision = Judgement.SELL
            } else {
              println(p.time + " price " + p.price + " decline. Do buy for lowV!")
              decision = Judgement.BUY
            }
            buysell = Judgement.SELL
            prevPrice = p.price
          }
        case _ =>
          println("error: buysell is illegal state")
          decision = Judgement.STAY
      }
    }

    if (decision != Judgement.STAY) prevDecision = decision
    decision
  }

  def checkLowVolatility: Boolean = {
    val v = TechAnal.slides.take(10).map(s => s.min)
    v.max - v.min <= 20
  }
}
