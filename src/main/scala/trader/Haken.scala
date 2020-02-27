package trader

import java.io.{FileOutputStream, PrintWriter}
import java.time.temporal.ChronoUnit

class Haken(val metrics: Metrics) {
  val p: Price = metrics.data.head
  var direction = 0 // 値動きの方向
  var lapsTime: Long = -1L // 前回のHakenからの経過時間。前回が存在しない場合は-1
  var worstDecline = 0.0 // 次のHakenが出現するまでの最悪低下幅
}

object Haken {
  private val thres = Settings.hakenThreshold
  private[trader] var hakens = List.empty[Haken]
  private var prevPrice = 0.0
  private[trader] var newHaken: Boolean = false
  private[trader] var runlen = 0

  def reset(): Unit = {
    hakens = List.empty[Haken]
    prevPrice = 0.0
    newHaken = false
  }

  /**
   * 最初を除き、新しいHakenが出現したらtrue
   */
  def add(metrics: List[Metrics]): Boolean = {
    val m = metrics.head
    val p = m.data.head

    val itvl = TechAnal.registerPrice(p.askPrice, p.time)
    if (itvl > thres) {
      if (hakens.nonEmpty) newHaken = true
      val lapsTime = if (hakens.isEmpty) Long.MaxValue
      else hakens.head.metrics.data.head.time.until(p.time, ChronoUnit.SECONDS)
      val h = new Haken(m)
      runlen += 1
      h.lapsTime = if (lapsTime > thres) -1L else lapsTime
      if (prevPrice != 0.0) {
        h.direction = (p.askPrice - prevPrice).signum
        if (h.direction != hakens.head.direction) {
          h.lapsTime = -1L
          runlen = 1
        }
      }
      hakens = h :: hakens
    } else {
      newHaken = false
      if (hakens.nonEmpty) {
        val h = hakens.head
        val diff = p.askPrice - h.metrics.data.head.askPrice
        if ((diff - h.worstDecline).signum == -h.direction) h.worstDecline = diff
      }
    }
    prevPrice = p.askPrice
    newHaken
  }

  def save(append: Boolean = true): Unit = {
    val writer = new PrintWriter(new FileOutputStream("hakens", append))
    hakens.foreach(h => {
      val m = h.metrics
      val p = m.data.head
      writer.print(p.time +"\t")
      writer.print(p.askPrice +"\t")
      writer.print(h.direction +"\t")
      writer.print(h.lapsTime +"\t")
      writer.print(h.worstDecline +"\t")
      writer.print(List(f"${m.amtrate}%.1f", f"${m.m5}%.1f", f"${m.m10}%.1f", f"${m.m20}%.1f", f"${m.m40}%.1f").mkString("\t"))
      writer.println("\t"+ m.stage)

    })
    writer.close()
  }
}