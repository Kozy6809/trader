package trader

/**
  * 売買判断の列挙値
  */
object Judgement extends Enumeration(-2) {
  val SELL, SETTLE_SELL, STAY, SETTLE_BUY, BUY = Value
}
