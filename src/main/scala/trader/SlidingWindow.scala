package trader

import java.time.LocalDateTime

class SlidingWindow(val startTime: LocalDateTime, val initialPrice: Double, amt: Int) {
  val limit = 5
  var prices = scala.collection.mutable.Map.empty[Double, Int]
  prices.put(round5(initialPrice), amt)

  def max: Double = prices.keys.max
  def min: Double = prices.keys.min

  /**
    * 価格を日経miniの呼び値(5円)に丸める
    */
  private def round5(d: Double) = (d / 5.0).round * 5.0

  def add(price: Double, amt: Int): Boolean = {
    val price5 = round5(price)
    if (price5 > min + limit || price5 < max - limit) false
    else {
      val prev = prices.getOrElse(price5, 0)
      prices.put(price5, prev + amt)
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
