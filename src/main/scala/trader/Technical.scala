package trader

import java.io.{BufferedReader, InputStreamReader, Reader}
import java.nio.file.{Files, Path, Paths}
import java.time.LocalTime
import java.time.temporal.ChronoUnit
import java.util.function.Consumer

import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.concurrent.duration._

import scala.annotation.tailrec
import java.time.LocalDateTime

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
  private var nightSession = remainSecInDuration(15, 45, 6, 0) > 0
  var loginTime: LocalDateTime = _


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
    (t.compareTo(LocalTime.of(5, 40)) > 0 && t.compareTo(LocalTime.of(6, 1)) < 0) ||
      (t.compareTo(LocalTime.of(15, 25)) > 0 && t.compareTo(LocalTime.of(15, 46)) < 0)
  }

  def isClosetime(t: LocalTime = LocalTime.now): Boolean = {
    if (nightSession) t.compareTo(LocalTime.of(5, 50)) > 0
    else t.compareTo(LocalTime.of(15, 35)) > 0
  }

  def main(args: Array[String]): Unit = {
    // -iオプションがあれば直ちにセッション開始するがさもなくば次のセッションから開始する
    if (!(args.length > 0 && args(0) == "-i")) {
      val inNight = remainSecInDuration(17, 0, 5, 55)
      if (inNight > 0) Thread.sleep(inNight)
      val inDay = remainSecInDuration(8, 45, 15, 40)
      if (inDay > 0) Thread.sleep(inDay)
    }

    SBIFutureHandler.attemptGenDriver()

    val stdinr = new InputStreamReader(System.in)
    if (Settings.showPrices) PriceWindow.init()
    def waitForMarket(): Unit = {
      // 時刻が5:55から8:45の間は8:45までスリープ
      Thread.sleep(remainSecInDuration(5, 55, 8, 45))
      // 時刻が15:40から17:00の間は17:00までスリープ
      Thread.sleep(remainSecInDuration(15, 40, 17, 0))
      Thread.sleep(3000L)
    }
    waitForMarket()

    handler.login()
    loginTime = LocalDateTime.now()


    var stopRequest = false
    while (!stopRequest) {
      while (stdinr.ready()) stopRequest = userAction(stdinr.read())
        // ログイン後1時間で強制ログアウトされエラー処理に入ってしまうので、その前にログアウト-再ログインする
        val elapsedTime = loginTime.until(LocalDateTime.now(), ChronoUnit.MINUTES)
        if (elapsedTime >= 55) {
          handler.logout()
          handler.close() // ログイン時に新たなドライバを起動するのでその前にクローズする。さもないと6個目のドライバ起動でクラッシュする
          handler.login()
          loginTime = LocalDateTime.now()
        }
        trading()

      if (!nightSession && remainSecInDuration(15, 40, 17, 0) > 0) {
        TechAnal.save()
        handler.close()
        TechAnal.reset()
        waitForMarket()
        nightSession = true
        handler.login()
      }

      if (nightSession && remainSecInDuration(5, 55, 8,45) > 0) stopRequest = true
    }
    TechAnal.save()
    handler.close()
    System.exit(0)
  }

  def printPrices(p: Price): Unit = {
    val m = Metrics.metrics.head
    val amtrate = f"${m.amtrate}%.1f"
    val m320 = f"${m.m5}%.1f"
    val m640 = f"${m.m10}%.1f"
    val m1280 = f"${m.m20}%.1f"
    val m2560 = f"${m.m40}%.1f"
    println(List(p.price, p.askPrice, p.amt, amtrate, m320, m640, m1280, m2560).mkString("\t"))
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
    val acquireFuture = Future[Price] {
      handler.acquirePrice()
    }
    try {
      val p = Await.result(acquireFuture, 10.seconds)
      if (TechAnal.add(p)) {
        printPrices(p)
      }
      Thread.sleep(1000) // 間隔が短いとbangされる
    } catch {
      case e: Exception => {
        var msg = e.getMessage()
        if (msg == null) msg = e.toString()
        else msg = msg.split("\n")(0)

        StockLogger.writeMessage(s"Technical:trading ${msg}")
        retryLogin()
      }
    }
  }

  def showPrices(data: List[Price] = TechAnal.data, metrics: List[Metrics] = Metrics.metrics) :Unit = {
    val p = data.head
    val prev = if (data.size > 1) data(1) else p
    val m = metrics.head
    val diffma = TechAnal.maDiff(m = metrics)
    val diffamt = TechAnal.amtDiff(d = data)
    PriceWindow.setData(p.time, p.askPrice, prev.askPrice, m.m5, m.m10, m.m20, m.m40,
      diffma._1, diffma._2, diffma._3, diffma._4, m.amtrate, diffamt)
    PriceWindow.repaint()
  }

  def replayShowPrices(): Unit = {
    PriceWindow.init()
    var metricses =  List.empty[List[Metrics]]
    val metrics = Metrics.metrics
    @tailrec
    def mkMetricses(m: List[Metrics]): Unit = {
      m match {
        case List() =>
        case x :: xs => metricses = m :: metricses; mkMetricses(xs)
      }
    }
    mkMetricses(metrics)
    var price: Double = 0.0
    metricses.foreach(m => {
      showPrices(m.head.data, m)
      if (price != m.head.data.head.askPrice) {
        Thread.sleep(100L)
        price = m.head.data.head.askPrice
      } else Thread.sleep(1L)
    })
  }

  def replay(fileName: String): Unit = {
    PriceWindow.init()
    val path = Paths.get(fileName)
    TechAnal.replay(path)
//    Haken.save()
//    TechAnal.save()
//    TechAnal.savePeaks()
//    TechAnal.saveSlides("slides")
    StockLogger.close()
    System.exit(0)
  }

  def replayAll(folderName: String): Unit = {
    object Proc extends Consumer[Path] {
      override def accept(t: Path): Unit = {
        println(t)
        StockLogger.bsMessage(t.toString)
        TechAnal.replay(t)
        Haken.save()
        TechAnal.reset()
        StrategyEvaluator.reset()
        Haken.reset()
      }
    }

    val path = Paths.get(folderName)
    Files.newDirectoryStream(path, "techs*").forEach(Proc)
    StockLogger.close()
    System.exit(0)
  }
}
