package trader

import java.time.LocalDateTime

class SlidingWindow(val data: List[Price]) {
  private val limit = 5
  private var prices = scala.collection.mutable.Map.empty[Double, Int]
  prices.put(round5(data.head.askPrice), diffAmt(data))

  def max: Double = prices.keys.max
  def min: Double = prices.keys.min
  private def diffAmt(data: List[Price]): Int = data.head.amt - (if (data.length > 1) data(1).amt else 0)

  /**
    * 価格を日経miniの呼び値(5円)に丸める
    */
  private def round5(d: Double) = (d / 5.0).round * 5.0

  /**
    * 与えられた価格が保持できるならtrueを返す
    */
  def add(data: List[Price]): Boolean = {
    val price5 = round5(data.head.askPrice)
    if (price5 > min + limit || price5 < max - limit) false
    else {
      val s = prices.size
      val prev = prices.getOrElse(price5, 0)
      prices.put(price5, prev + diffAmt(data))
      if (prices.size > s) SlidingWindow.range = SlidingWindow.calcRange()
      true
    }
  }

  def result: List[Double] = {
    var r = List.empty[Double]
    val keys = prices.keys.toList.sorted
    for (key <- keys) {
      r = key :: r
      r = prices(key) :: r
    }
    r.reverse
  }

}

object SlidingWindow {
  var slides = List.empty[SlidingWindow]
  var range: (Double, Double) = (0.0, 0.0)

  /**
    * 与えられた価格が現在のウィンドウで保持できるならウィンドウに付加してtrueを返す
    * さもなくば新しいウィンドウを生成してfalseを返す
    */
  def add(data: List[Price]): Boolean = {
    if (slides.isEmpty || !slides.head.add(data)) {
      slides = new SlidingWindow(data) :: slides
      range = calcRange()
      false
    } else true
  }

  def reset(): Unit = slides = List.empty[SlidingWindow]

  def calcRange(target: List[SlidingWindow] = slides, duration: Long = 15): (Double, Double) = {
    val till = target.head.data.head.time.minusMinutes(duration)
    val frac = target.takeWhile(s => s.data.head.time.isAfter(till))

    val min = frac.map(s => s.min).min
    val max = frac.map(s => s.max).max
    (min, max)
  }
}