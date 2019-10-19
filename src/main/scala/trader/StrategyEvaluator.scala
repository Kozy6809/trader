package trader

object StrategyEvaluator extends Strategy {
  val s:Strategy = RangeStrategy
  var holding: Judgement.Value = Judgement.STAY
  var price = 0.0
  var gain = 0.0

  def reset() = {
    s.reset()
    holding = Judgement.STAY
    price = 0.0
    gain = 0.0
  }

  override def add(p: Price): Judgement.Value = {
    var decision = s.add(p)
    holding match {
      case Judgement.SELL =>
        decision match  {
          case Judgement.BUY => holding = Judgement.BUY; gain += price - p.price - 5; price = p.price
          case Judgement.SETTLE_BUY => holding = Judgement.STAY; gain += price - p.price - 5; price = 0.0
          case Judgement.SELL => decision = Judgement.STAY
          case Judgement.SETTLE_SELL => println(p.time + " error: not buying, now selling")
          case _ =>
        }
      case Judgement.BUY => {
        decision match {
          case Judgement.BUY => decision = Judgement.STAY
          case Judgement.SETTLE_BUY => println(p.time + " error: not selling, now buying")
          case Judgement.SELL => holding = Judgement.SELL; gain += p.price - price - 5; price = p.price
          case Judgement.SETTLE_SELL => holding = Judgement.STAY; gain += p.price - price - 5; price = 0.0
          case _ =>
        }
      }
      case _ => { // STAY
        decision match {
          case Judgement.BUY => holding = Judgement.BUY; price = p.price
          case Judgement.SETTLE_BUY => println(p.time + "error: currently not holding")
          case Judgement.SELL => holding = Judgement.SELL; price = p.price
          case Judgement.SETTLE_SELL => println(p.time + "error: currently not holding")
          case _ =>
        }
      }

    }
    if (decision != Judgement.STAY) StockLogger.bsMessage(p.time +"\t"+ p.price +"\t"+ gain +"\t"+ decision)

    decision

  }
}
