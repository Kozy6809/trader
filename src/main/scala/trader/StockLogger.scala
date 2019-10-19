package trader

import java.io.{File, PrintWriter}
import java.time.LocalTime

object StockLogger {
  val writer = new PrintWriter(new File("trade.log"))
  val bswriter = new PrintWriter(new File("bs2.log"))

  def write(at: LocalTime, code: Int, kind: trader.Judgement.Value, price: Double) = {
    if (kind != Judgement.STAY) writer.println(at, code, kind, price)
  }

  def writeMessage(msg: String) = {
    writer.println(LocalTime.now() +" "+ msg)
    writer.flush()
  }
  def bsMessage(msg: String) = {
    bswriter.println(LocalTime.now() +" "+ msg)
    bswriter.flush()
  }

  def close() = {
    writer.close()
    bswriter.close()
  }

}
