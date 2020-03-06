package trader

import java.io.{FileOutputStream, PrintWriter}
import java.time.temporal.ChronoUnit

class Haken(val metrics: Metrics) {
  val p: Price = metrics.data.head
  var direction = 0 // 値動きの方向
  var lapsTime: Long = -1L // 前回のHakenからの経過時間。前回が存在しない場合は-1
  var worstDecline = 0.0 // 次のHakenが出現するまでの最悪低下幅
  var stageCount = 0 // Haken間のステージ切り替わり回数
}

object Haken {
  private val thres = Settings.hakenThreshold
  private[trader] var hakens = List.empty[Haken]
  private var prevPrice = 0.0
  private var prevStage = 0
  private var stageCount = 0
  private[trader] var newHaken: Boolean = false
  private[trader] var runlen = 0
  private[trader] var varWorstDecline = 0.0

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

    if (m.stage != prevStage) {
      stageCount += 1
      prevStage = m.stage
    }
    val itvl = TechAnal.registerPrice(p.askPrice, p.time)
    if (itvl > thres) {
      if (hakens.nonEmpty) newHaken = true
      val lapsTime = if (hakens.isEmpty) Long.MaxValue
      else hakens.head.metrics.data.head.time.until(p.time, ChronoUnit.SECONDS)
      val h = new Haken(m)
      h.stageCount = stageCount
      stageCount = 0
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
      varWorstDecline = varwd()
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

  /**
    * 直近のシーケンスのdirectionがstageに照らして適切かどうかの指数を返す
    * directionが全て同じかどうかのチェックは行わない
    */
  def stagePenalty(len: Int): Int = {
    def stageMap(h: Haken): Int = {
      val stage = h.metrics.stage
      if (h.direction >= 0) {
        if (stage == 4) -2
        else if (stage == 3 || stage == 5) -1
        else 0
      } else {
        if (stage == 1) -2
        else if (stage == 6 || stage == 2) -1
        else 0
      }
    }

    hakens.take(len).map(stageMap).sum
  }

  /**
    * worstDeclineの分散
    */
  private def varwd(span: Long = 3600): Double = {
    val from = hakens.head.p.time.minusSeconds(span)
    val slice = hakens.takeWhile(h => h.p.time.isAfter(from))
    val n = slice.length
    if (n == 1) 0.0
    else {
      val wd = slice.map(h => h.worstDecline)
      val mean = wd.sum / n
      wd.map(w => (w - mean) * (w - mean)).sum / (n - 1)
    }
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
      writer.print(List(f"${m.amtrate}%.1f", f"${m.m5}%.1f", f"${m.m10}%.1f", f"${m.m20}%.1f", f"${m.m40}%.1f",
        f"${m.m5 - m.m20}%.1f", f"${m.m20 - m.m40}%.1f").mkString("\t"))
      writer.println("\t"+ m.stage +"\t"+ h.stageCount)

    })
    writer.close()
  }
}