package trader

import java.io.{File, PrintWriter}
import java.time.LocalTime
import java.time.ZoneId

object StockLogger {
  private val replaymode = Settings.replaymode
  private val writer: PrintWriter = if (replaymode) null else new PrintWriter(new File("trade.log"))
  private val bsLogName = if (replaymode) "bsreplay.log" else "bs.log"
  private val bswriter = new PrintWriter(new File(bsLogName))

  def write(at: LocalTime, code: Int, kind: trader.Judgement.Value, price: Double): Unit = {
    if (!replaymode && kind != Judgement.STAY) writer.println(at, code, kind, price)
  }

  def writeMessage(msg: String): Unit = {
    if (!replaymode) {
      writer.println(s"${LocalTime.now(ZoneId.of("Asia/Tokyo"))} $msg")
      writer.flush()
    }
  }
  def bsMessage(msg: String): Unit = {
    bswriter.println(s"${LocalTime.now(ZoneId.of("Asia/Tokyo"))} $msg")
    bswriter.flush()
  }

  def close(): Unit = {
    if (!replaymode) writer.close()
    bswriter.close()
  }

}
