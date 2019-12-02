package trader

import java.io.{BufferedReader, InputStreamReader, Reader}
import java.nio.file.{Files, Path, Paths}
import java.time.LocalTime
import java.util.function.Consumer

/**
  * 値動きを追跡して適切なアクションを実行する
  * 価格情報ストリームと建玉情報に基づき、売買アクションを起こす
  * タイムレゾリューションは選択できる(1分、5分…)
  * 移動平均の計算
  * トレンドの判定
  * トレンド読み違え時の対処
  * ロギング
  */
object Technical {
  private val handler = SBIFutureHandler
  val tradeUnit = 1
  private val nightSession = LocalTime.now.compareTo(LocalTime.of(16, 30)) > 0 ||
    LocalTime.now.compareTo(LocalTime.of(5, 30)) < 0

  def isClosing(t: LocalTime = LocalTime.now): Boolean = {
    (t.compareTo(LocalTime.of(5, 10)) > 0 && t.compareTo(LocalTime.of(5, 31)) < 0) ||
      (t.compareTo(LocalTime.of(14, 55)) > 0 && t.compareTo(LocalTime.of(15, 11)) < 0)
  }

  def isClosetime(t: LocalTime = LocalTime.now): Boolean = {
    if (nightSession) t.compareTo(LocalTime.of(5, 20)) > 0
    else t.compareTo(LocalTime.of(15, 5)) > 0
  }

  def main(args: Array[String]): Unit = {
    if (args.length > 0) {
      if (args(0) == "-f") replayAll(args(1))
      else replay(args(0))
      System.exit(0)
    }
    var daySessionSaved = false

    val isr = new InputStreamReader(System.in)

    handler.login()

    var stopRequest = false
    var i = 1
    while (!stopRequest) {
      try {
        val p = handler.acquirePrice()
        if (TechAnal.add(p)) {
          val m = TechAnal.metrics.head
          val amtrate = f"${m._1}%.1f"
          val m320 = f"${m._2}%.1f"
          val m640 = f"${m._3}%.1f"
          val m1280 = f"${m._4}%.1f"
          val m2560 = f"${m._5}%.1f"
          println(List(i, p.price, p.amt.get, amtrate, m320, m640, m1280, m2560).mkString("\t"))
          i += 1
        }

        Thread.sleep(1000) // 間隔が短いとbangされる
      } catch {
        case e: Exception =>
          StockLogger.writeMessage(s"Technical:33 ${e.getMessage} ${e.getClass}")
          var done = false
          while (!done) {
            try {
              handler.close()
              handler.login()
              done = true
            } catch {
              case e: Exception => StockLogger.writeMessage(s"Technical:41 ${e.getMessage}")
            }
          }
      }
      while (isr.ready()) {
        isr.read() match {
          // w = 119 q = 113 a = 97 s = 115 z = 122 x = 120
          case 119 => TechAnal.save()
          case 113 => stopRequest = true
//          case 97 => {handler.orderNewSell(tradeUnit); println("新規売 " + tradeUnit)}
//          case 115 => {handler.orderNewBuy(tradeUnit); println("新規買 " + tradeUnit)}
//          case 122 => {handler.orderSettleSell(tradeUnit); println("決済売 " + tradeUnit)}
//          case 120 => {handler.orderSettleBuy(tradeUnit); println("決済買 " + tradeUnit)}
          case _ =>
        }
      }
      if (nightSession && LocalTime.now.compareTo(LocalTime.of(5,35)) > 0 &&
        LocalTime.now.compareTo(LocalTime.of(5, 55)) < 0) stopRequest = true
      if (!nightSession && LocalTime.now.compareTo(LocalTime.of(15, 20)) > 0) {
//        TechAnal.save()
//        daySessionSaved = true
        stopRequest = true
      }
    }
    TechAnal.save()
    handler.close()
  }

  def replay(fileName: String): Unit = {
    val path = Paths.get(fileName)
    TechAnal.replay(path)
    TechAnal.savePeaks()
    TechAnal.saveSlides("slides")
    StockLogger.close()
  }

  def replayAll(folderName: String): Unit = {
    object Proc extends Consumer[Path] {
      override def accept(t: Path): Unit = {
        println(t)
        StockLogger.bsMessage(t.toString)
        TechAnal.replay(t)
        TechAnal.reset()
        StrategyEvaluator.reset()
      }
    }

    val path = Paths.get(folderName)
    Files.newDirectoryStream(path, "techs*").forEach(Proc)
    StockLogger.close()
  }
}
