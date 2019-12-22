package trader

import java.time.LocalDateTime

/**
  * ある時点での価格情報を保持するクラス
  */
class Price(
  val time: LocalDateTime,
  val price: Double,
  val askPrice: Double, // 売り気配値
  val amt: Int // 出来高
)

