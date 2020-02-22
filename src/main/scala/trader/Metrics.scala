package trader

import java.time.temporal.ChronoUnit

/**
  * 現在の株価から計算される統計情報を保持するクラス
  * 現在保持しているのは:
  * 出来高/sec
  * 移動平均(320, 640, 1280, 2560sec)
  */
class Metrics(val data: List[Price], val slides: List[SlidingWindow]) {
  private[trader] val amtrate = if (data.tail.isEmpty) 0.0
  else (data.head.amt - data(1).amt) / (data(1).time.until(data.head.time, ChronoUnit.MILLIS) / 1000.0)
  private val mas: Array[Double] = Array.ofDim(4)
  mas(0) = ema(0, 300)
  mas(1) = ema(1, 600)
  mas(2) = ema(2, 1200)
  mas(3) = ema(3, 2400)
  private[trader] def m5 = mas(0)
  private[trader] def m10 = mas(1)
  private[trader] def m20 = mas(2)
  private[trader] def m40 = mas(3)

  def sma(period: Long): Double = {
    val from = data.head.time.minusSeconds(period)
    val dataSlice = data.takeWhile(p => p.time.compareTo(from) >= 0)
    val sum = dataSlice.map(t => t.price).sum
    sum / dataSlice.length
  }

  def ema(i:Int, period: Long): Double = {
    val from = data.head.time.minusSeconds(period)
    val dataSlice = Metrics.metrics.takeWhile(m => m.data.head.time.compareTo(from) >= 0)
    val latest = dataSlice.head.mas(i)
    val sum = dataSlice.map(m => m.mas(i)).sum + latest
    sum / (dataSlice.length + 1)
  }

}

object Metrics {
  var metrics = List.empty[Metrics]

  def add(data: List[Price]): Unit = {
    val m = new Metrics(data, SlidingWindow.slides)
    metrics = m :: metrics
  }

  def reset():Unit = {
    metrics = List.empty[Metrics]
  }
}