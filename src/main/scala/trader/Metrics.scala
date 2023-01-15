package trader

import java.time.temporal.ChronoUnit

/**
  * 現在の株価から計算される統計情報を保持するクラス
  * 現在保持しているのは:
  * 出来高/sec → 出来高の60秒emaに変更(2023/1/15)
  * 売値の移動平均(300, 600, 1200, 2400sec)
  */
class Metrics(val data: List[Price]) {
  private val mas: Array[Double] = Array.ofDim(5)
  mas(0) = ema(0, 300)
  mas(1) = ema(1, 600)
  mas(2) = ema(2, 1200)
  mas(3) = ema(3, 2400) // ここまで売値のema
  mas(4) = ema(4, 60) // 出来高のema
  private[trader] def m5 = mas(0)
  private[trader] def m10 = mas(1)
  private[trader] def m20 = mas(2)
  private[trader] def m40 = mas(3)
  private[trader] def amtrate = mas(4)

  private[trader] def maRange: Double = mas.max - mas.min
  private[trader] def rangeDistance(p: Double): Double = {
    val u = mas.max
    val l = mas.min
    if (p > l && p < u) 0.0
    else Math.min((p - u).abs, (p - l).abs)
  }
  private[trader] val stage = if (m5 >= m20 && m20 >= m40) 1
  else if (m20 >= m5 && m5 >= m40) 2
  else if (m20 >= m40 && m40 >= m5) 3
  else if (m40 >= m20 && m20 >= m5) 4
  else if (m40 >= m5 && m5 >= m20) 5
  else 6

  def ema(i:Int, period: Long): Double = {
    // i == 4なら出来高の、それ以外なら売値のemaを求める
    val v = if (i == 4) data.head.amt else data.head.askPrice
    if (Metrics.metrics.isEmpty) v
    else {
      val prev = Metrics.metrics.head.mas(i)
      val from = data.head.time.minusSeconds(period)
      val nprev = Metrics.metrics.takeWhile(m => m.data.head.time.compareTo(from) >= 0).length
      val k = 2.0 / (nprev + 1)
      prev + k * (v - prev)
    }
  }

}

object Metrics { // Metricsのリストを保持する
  var metrics = List.empty[Metrics]

  def add(data: List[Price]): Unit = {
    val m = new Metrics(data)
    metrics = m :: metrics
  }

  def reset():Unit = {
    metrics = List.empty[Metrics]
  }
}