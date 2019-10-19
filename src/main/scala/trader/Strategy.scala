package trader

trait Strategy {
  def add(p: Price):Judgement.Value
  def reset(): Unit
}
