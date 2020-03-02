package trader

object StrategyEvaluator extends Strategy {
  val s:Strategy = HakenSpeculativeStrategy
  var holding: Judgement.Value = Judgement.STAY
  var price = 0.0 // 建玉の価格。建玉を決済した時はその時点の価格
  var gain = 0.0 // 累積利得

  def reset(): Unit = {
    s.reset()
    holding = Judgement.STAY
    price = 0.0
    gain = 0.0
  }

  override def add(p: Price): Judgement.Value = {
    val sellPrice = p.askPrice - 5
    val buyPrice = p.askPrice
    var decision = s.add(p)
    def settle(d: Double, p: Double): Unit = {gain += d; price = p}
    holding match {
      case Judgement.SELL =>
        decision match  {
          case Judgement.BUY => holding = Judgement.BUY; settle(price - buyPrice, buyPrice)
          case Judgement.SETTLE_BUY => holding = Judgement.STAY; settle(price - buyPrice, buyPrice)
          case Judgement.SELL => decision = Judgement.STAY
          case Judgement.SETTLE_SELL => println(p.time + " error: not buying, now selling")
          case _ =>
        }
      case Judgement.BUY =>
        decision match {
          case Judgement.BUY => decision = Judgement.STAY
          case Judgement.SETTLE_BUY => println(p.time + " error: not selling, now buying")
          case Judgement.SELL => holding = Judgement.SELL; settle(sellPrice - price, sellPrice)
          case Judgement.SETTLE_SELL => holding = Judgement.STAY; settle(sellPrice - price, sellPrice)
          case _ =>
        }
      case _ => // STAY
        decision match {
          case Judgement.BUY => holding = Judgement.BUY; price = buyPrice
          case Judgement.SETTLE_BUY => println(p.time + "error: currently not holding")
          case Judgement.SELL => holding = Judgement.SELL; price = sellPrice
          case Judgement.SETTLE_SELL => println(p.time + "error: currently not holding")
          case _ =>
        }

    }
    if (decision != Judgement.STAY) StockLogger.bsMessage(p.time +"\t"+ price +"\t"+ gain +"\t"+ decision)

    decision

  }
}
