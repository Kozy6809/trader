package trader

import java.time.temporal.ChronoUnit

/**
  * 現在の株価から計算される統計情報を保持するクラス
  * 現在保持しているのは:
  * 出来高/sec
  * 移動平均(320, 640, 1280, 2560sec)
  */
class Metrics(val data: List[Price], val slides: List[SlidingWindow]) {
  private[trader] val amtrate = if (data.length < 2) 0.0
  else (data.head.amt - data(1).amt) / (data(1).time.until(data.head.time, ChronoUnit.MILLIS) / 1000.0)
  private[trader] val m320 = ma(320)
  private[trader] val m640 = ma(640)
  private[trader] val m1280 = ma(1280)
  private[trader] val m2560 = ma(2560)

  def ma(period: Long): Double = {
    if (data.length < 2) data.head.price
    else {
      val from = data.head.time.minusSeconds(period)
      val lagged = data.tail
      val zipped = data zip lagged
      val dataSlice = zipped.filter(p => p._1.time.compareTo(from) >= 0)
      val sum = dataSlice.map(t => t._1.price).sum
      sum / dataSlice.length
    }
  }

}
