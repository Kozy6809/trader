package trader

import java.io.{BufferedReader, InputStreamReader, Reader}
import java.nio.file.{Files, Path, Paths}
import java.time.LocalTime
import java.time.temporal.ChronoUnit
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
  private var nightSession = remainSecInDuration(15, 15, 5,30) > 0

  /**
    * 指定された時刻が指定された範囲に入っているなら、範囲終端までのミリ秒数を返す。範囲外なら0を返す
    * 範囲は開区間であり、範囲に端点は含まれない
    * 範囲が0時0分をまたぐ場合の処理も正しく行う
    * 時刻が正確に0時0分の場合必ず範囲外とみなされてしまう
    */
    def remainSecInDuration(h1: Int, m1: Int, h2: Int, m2: Int, t: LocalTime = LocalTime.now): Long = {
      val d0 = LocalTime.of(0, 0)
      val d1 = LocalTime.of(h1, m1)
      val d2 = LocalTime.of(h2, m2)
      var res = 0L
      if (d2.isAfter(d1)) {
        res = if (t.isAfter(d1) && t.isBefore(d2)) t.until(d2, ChronoUnit.MILLIS) else 0L
      } else {
        if (t.isAfter(d0) && t.isBefore(d2)) res = t.until(d2, ChronoUnit.MILLIS)
        else if (t.isAfter(d1)) res = 24 * 3600 * 1000 - d0.until(t, ChronoUnit.MILLIS) + d0.until(d2, ChronoUnit.MILLIS)
        else res = 0L
      }

      res
    }

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

    val stdinr = new InputStreamReader(System.in)
    PriceWindow.init()
    // 時刻が5:25から8:46の間は8:46までスリープ
    Thread.sleep(remainSecInDuration(5, 25, 8, 46) )
    // 時刻が15:10から16:31の間は16:31までスリープ
    Thread.sleep(remainSecInDuration(15, 10, 16, 31) )

    handler.login()

    var stopRequest = false
    while (!stopRequest) {
      while (stdinr.ready()) stopRequest = userAction(stdinr.read())

      trading()

      if (!nightSession && remainSecInDuration(15, 18, 15,19) > 0) {
        TechAnal.save()
        handler.close()
        TechAnal.reset()
        Thread.sleep(remainSecInDuration(15, 18, 16,31))
        nightSession = true
        handler.login()
      }

      if (nightSession && remainSecInDuration(5, 33, 5,34) > 0) stopRequest = true
    }
    TechAnal.save()
    handler.close()
    System.exit(0)


    def printPrices(p: Price): Unit = {
      val m = TechAnal.metrics.head
      val amtrate = f"${m.amtrate}%.1f"
      val m320 = f"${m.m320}%.1f"
      val m640 = f"${m.m640}%.1f"
      val m1280 = f"${m.m1280}%.1f"
      val m2560 = f"${m.m2560}%.1f"
      println(List(p.price, p.askPrice, p.amt, amtrate, m320, m640, m1280, m2560).mkString("\t"))
    }

    def showPrices() :Unit = {
      val p = TechAnal.data.head
      val prev = if (TechAnal.data.size > 1) TechAnal.data(1) else p
      val m = TechAnal.metrics.head
      val diffma = TechAnal.maDiff()
      PriceWindow.setData(p.askPrice, prev.askPrice, m.m320, m.m640, m.m1280, m.m2560,
        diffma._1, diffma._2, diffma._3, diffma._4, m.amtrate)
      PriceWindow.repaint()
    }
    def retryLogin(): Unit = {
      var done = false
      while (!done) {
        try {
          handler.close()
          handler.login()
          done = true
        } catch {
          case e: Exception => StockLogger.writeMessage(s"Technical:retryLogin ${e.getMessage}")
        }
      }
    }

    def userAction(keyCode: Int): Boolean = {
      var res = stopRequest
       keyCode match {
        // w = 119 q = 113 a = 97 s = 115 z = 122 x = 120
        case 119 => TechAnal.save()
        case 113 => res = true
        //          case 97 => {handler.orderNewSell(tradeUnit); println("新規売 " + tradeUnit)}
        //          case 115 => {handler.orderNewBuy(tradeUnit); println("新規買 " + tradeUnit)}
        //          case 122 => {handler.orderSettleSell(tradeUnit); println("決済売 " + tradeUnit)}
        //          case 120 => {handler.orderSettleBuy(tradeUnit); println("決済買 " + tradeUnit)}
        case _ =>
      }
      res
    }

    def trading(): Unit = {
      try {
        val p = handler.acquirePrice()
        if (TechAnal.add(p)) {
          printPrices(p)
          showPrices()
        }
        Thread.sleep(1000) // 間隔が短いとbangされる
      } catch {
        case e: Exception => retryLogin()
      }
    }
  }

  def replay(fileName: String): Unit = {
    val path = Paths.get(fileName)
    TechAnal.replay(path)
    TechAnal.save()
//    TechAnal.savePeaks()
//    TechAnal.saveSlides("slides")
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
