package trader

import java.time.LocalDateTime

/**
  * ある時点での価格情報を保持するクラス
  */
class Price(
  val time: LocalDateTime,
  val price: Double,
  val amt: Option[Int] // 出来高
)

