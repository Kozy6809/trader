package trader

import java.time.LocalDateTime

class SlidingWindow(val startTime: LocalDateTime, val initialPrice: Double, amt: Int) {
  val limit = 10
  var prices = scala.collection.mutable.Map.empty[Double, Int]
  prices.put(initialPrice, amt)

  def max: Double = prices.keys.max
  def min: Double = prices.keys.min

  def add(price: Double, amt: Int): Boolean = {
    if (price > min + limit || price < max - limit) false
    else {
      val prev = prices.getOrElse(price, 0)
      prices.put(price, prev + amt)
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
