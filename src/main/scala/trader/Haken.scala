package trader

import java.io.PrintWriter
import java.time.temporal.ChronoUnit

class Haken(val p: Price) {
  var direction = 0 // 値動きの方向
  var lapsTime: Long = -1L // 前回のHakenからの経過時間。前回が存在しない場合は-1
  var worstDecline = 0.0 // 次のHakenが出現するまでの最悪低下幅
}

object Haken {
  private val thres = Settings.hakenThreshold
  private[trader] var hakens = List.empty[Haken]
  private var prevPrice = 0.0

  def reset(): Unit = {
    hakens = List.empty[Haken]
    prevPrice = 0.0
  }

  /**
   * 新しいHakenが出現したらtrue
   */
  def add(p: Price): Boolean = {
    val itvl = TechAnal.registerPrice(p.askPrice, p.time)
    if (itvl > thres) {
      val lapsTime = if (hakens.isEmpty) Long.MaxValue
      else hakens.head.p.time.until(p.time, ChronoUnit.SECONDS)
      val h = new Haken(p)
      h.lapsTime = if (lapsTime > thres) -1L else lapsTime
      if (prevPrice != 0.0) {
        h.direction = (p.askPrice - prevPrice).signum
        if (h.direction != hakens.head.direction) h.lapsTime = -1L
      }
      hakens = h :: hakens
      prevPrice = p.askPrice
      true
    } else {
      prevPrice = p.askPrice
      if (hakens.nonEmpty) {
        val h = hakens.head
        val diff = p.askPrice - h.p.askPrice
        if ((diff - h.worstDecline).signum == -h.direction) h.worstDecline = diff
      }
      false
    }
  }

  def save(): Unit = {
    val writer = new PrintWriter("hakens")
    hakens.foreach(h => {
      writer.print(h.p.time +"\t")
      writer.print(h.p.askPrice +"\t")
      writer.print(h.direction +"\t")
      writer.print(h.lapsTime +"\t")
      writer.println(h.worstDecline +"\t")
    })
    writer.close()
  }
}