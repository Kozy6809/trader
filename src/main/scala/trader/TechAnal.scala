package trader

import java.io.{FileReader, PrintWriter}
import java.nio.file.{Files, Path}
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit
import java.time.{LocalDateTime, LocalTime}
import java.util.function.Consumer

import scala.collection.mutable.ListBuffer

/**
  * 価格情報を蓄積して分析する
  */
object TechAnal {
  var data = List.empty[Price]
  var metrics = List.empty[(Double, Double, Double, Double, Double)]
  var slidingMetrics = List.empty[(Double, Double, Double, Double, Double)]
  var slides = List.empty[SlidingWindow]
  var replayMode = false
  var peaks = List.empty[(Int, Int)] // 直近のピーク位置, ピークをピークと認識した位置。Listの末尾からのインデックスなので要注意
  var upeaks = List.empty[(Int, Int)] // 上に凸なピーク
  var lpeaks = List.empty[(Int, Int)] // 下に凸なピーク
  var peakClosing = false // 次のピークが近づいている
  var peakDirection = 0.0 // 現在のピークに対する次のピークの符号
  var peakCandidatePos = 0
  var atPeakDetectedPrice = 0.0 // ピーク検出時点の価格
  private var prevRange = (0.0, 0.0)
  private var currentRange = (0.0, 0.0)
  private var newRange = false

  def reset(): Unit = {
    data = List.empty[Price]
    metrics = List.empty[(Double, Double, Double, Double, Double)]
    slides = List.empty[SlidingWindow]
    replayMode = false
    peaks = List.empty[(Int, Int)] // 直近のピーク位置, ピークをピークと認識した位置。Listの末尾からのインデックスなので要注意
    upeaks = List.empty[(Int, Int)] // 上に凸なピーク
    lpeaks = List.empty[(Int, Int)] // 下に凸なピーク
    peakClosing = false // 次のピークが近づいている
    peakDirection = 0.0 // 現在のピークに対する次のピークの符号
    peakCandidatePos = 0
    atPeakDetectedPrice = 0.0 // ピーク検出時点の価格
    prevRange = (0.0, 0.0)
    currentRange = (0.0, 0.0)
  }

  def add(p: Price): Boolean = {
    if (!replayMode) handleStall()

    val amt = if (data.length <= 1) 1 else data.head.amt.get - data(1).amt.get
    // 出来高が前回と同じなら記録しない
    if (data.nonEmpty && p.amt.get == data.head.amt.get) false
    else {
      data = p :: data

      calcMetrics()

      if (slides.isEmpty) {
        slides = new SlidingWindow(p.time, p.price, amt) :: slides
        slidingMetrics = metrics.head :: slidingMetrics
        currentRange = swRange()
        prevRange = currentRange
      } else if (!slides.head.add(p.price, amt)) {
        slides = new SlidingWindow(p.time, p.price, amt) :: slides
        slidingMetrics = metrics.head :: slidingMetrics
        prevRange = currentRange
        currentRange = swRange()
        newRange = true
      }

      if (detectPeak()) {
        if (peakDirection > 0) upeaks = peaks.head :: upeaks
        else lpeaks = peaks.head :: lpeaks
        atPeakDetectedPrice = p.price
      }

      StrategyEvaluator.add(p)

      true
    }
  }

  private[trader] def calcMetrics() = {
    def ma(period: Long): (Double, Int) = {
      val from = data.head.time.minusSeconds(period)
      val lagged = data.tail
      val zipped = data zip lagged
      val dataSlice = zipped.filter(p => p._1.time.compareTo(from) >= 0)
      //      val sum = dataSlice.map(t => t._1.price * (t._1.amt.get - t._2.amt.get)).sum
      //      val totalamt = data.head.amt.get - dataSlice.last._2.amt.get
      //      sum / (if (totalamt == 0) 1 else totalamt)
      val sum = dataSlice.map(t => t._1.price).sum
      (sum / dataSlice.length, dataSlice.length)
    }

    val res = if (data.length == 1) {
      peaks = (0, 0) :: peaks
      val p = data.head.price
      (0.0, p, p, p, p)
    } else {
      val amtrate = (data.head.amt.get - data(1).amt.get) / (data(1).time.until(data.head.time, ChronoUnit.MILLIS) / 1000.0)
      val m320 = ma(320)._1
      val m640 = ma(640)._1
      val m1280 = ma(1280)._1
      val m2560 = ma(2560)._1
//      val m5120 = ma(5120)._1
//      val m10240 = ma(10240)._1
//      val m20480 = ma(20480)._1
      (amtrate, m320, m640, m1280, m2560)
    }
    metrics = res :: metrics
    res
  }

  /**
    * 直近2分間のm320, m1280, m2560の変化率を返す
    */
  private[trader] def maRate = {
    val t0 = data.head.time.minusSeconds(120)
    val at = data.count(p => p.time.compareTo(t0) >= 0)
    val m0 = metrics.head
    if (at > 0) {
      val m1 = metrics(at - 1)
      (m0._2 - m1._2, m0._4 - m1._4, m0._5 - m1._5)
    } else (0.0, 0.0, 0.0)
  }
  /**
    * ピークを検出する。前回ピークよりgap以上離れているピークを検出感度sensで検出する
    * ピークを認識したらtrueを返す
    */
  def detectPeak(gap: Double = 5, sens: Double = 0.8): Boolean = {
    var res = false
    val peakval = metrics(metrics.length - peaks.head._1 - 1)._2 // m320
    val currentval = metrics.head._2
    val diff = currentval - peakval

    if (Math.abs(diff) > gap) {
      if (peakClosing) {
        val peakCandidateVal = metrics(metrics.length - peakCandidatePos - 1)._2
        val positiveDiff = (currentval - peakCandidateVal) * peakDirection
        if (positiveDiff > 0) {
          peakCandidatePos = metrics.length - 1
        } else if (positiveDiff < -sens) { // peak detected
          //          println(peakDirection + "peak " + peakCandidatePos + " detected at " + (metrics.length - 1))
          peaks = (peakCandidatePos, metrics.length - 1) :: peaks
          res = true
          peakClosing = false
        }
      } else {
        peakClosing = true
        peakDirection = Math.signum(diff)
        peakCandidatePos = metrics.length - 1
      }
    } else peakClosing = false

    res
  }

  /**
    * 与えられた時刻における直近のピーク包絡線の延長位置を求める
    * まだピーク包絡線ができていない場合はNaNを返す
    */
  def envelopeExtention(at: LocalDateTime): (Double, Double) = {
    def anExtention(peaks: List[(Int, Int)], at: LocalDateTime): Double = {
      val p1 = metrics.length - peaks.head._1 - 1
      val p2 = metrics.length - peaks(1)._1 - 1
      val t1 = data(p1).time
      val t2 = data(p2).time
      val v1 = metrics(p1)._2 // m320
      val v2 = metrics(p2)._2
      (v1 - v2) / t2.until(t1, ChronoUnit.MILLIS) * t2.until(at, ChronoUnit.MILLIS) + v2
    }

    val r1 = if (upeaks.length <= 1) Double.NaN else anExtention(upeaks, at)
    val r2 = if (lpeaks.length <= 1) Double.NaN else anExtention(lpeaks, at)
    (r1, r2)
  }

  /**
    * 直近の上下計4つのピークから現在のトレンドを求める
    * 上下のピークの中点の、左右の差を計算するが、ピークがまだ少ない場合、上下左右のピークを共用する
    * ピーク数が0ならNaNを返す
    */
  def peakTrend: Double = {
    def aPeakVal(peaks: List[(Int, Int)], pos: Int): Double = {
      peaks.length match {
        case 0 => Double.NaN
        case 1 => metrics(metrics.length - peaks.head._1 - 1)._2 // m320
        case _ => metrics(metrics.length - peaks(pos)._1 - 1)._2
      }
    }

    val u1 = aPeakVal(upeaks, 0)
    val u2 = aPeakVal(upeaks, 1)
    val l1 = aPeakVal(lpeaks, 0)
    val l2 = aPeakVal(lpeaks, 1)
    (u1 + l1) / 2 - (u2 + l2) / 2
  }

  /**
    * 直近の価格レンジを返す
    */
  def swRange(span: Int = 10): (Double, Double) = {
    val sl = slides.take(span)
    val min = sl.map(s => s.min).min
    val max = sl.map(s => s.max).max
    (min, max)
  }

  def rangeTrend: Double = {
    val upperTrend = currentRange._2 - prevRange._2
    val lowerTrend = currentRange._1 - prevRange._1
    if (upperTrend * lowerTrend < 0) (currentRange._2 + currentRange._1) / 2 - (prevRange._2 + prevRange._1) / 2
    else if (upperTrend > 0.0) upperTrend
    else if (lowerTrend < 0.0) lowerTrend
    else 0.0
  }
  def isNewRange: Boolean = {
    val r = newRange
    newRange = false
    r
  }

  /**
    * 現在時刻文字列をファイル名として使用可能なものに変換する
    */
  def legalTimeStr: String = {
    val s = LocalDateTime.now.toString.replace(':', '_')
    s.substring(0, s.length - 4)
  }

  def save(): Unit = {
    val timeStr = legalTimeStr
    val writer = new PrintWriter("techs" + timeStr)

    data.zip(metrics).foreach(d => {
      val d1 = d._1
      val d2 = d._2
      writer.println(List(d1.time, d1.price, d1.amt.get, f"${d2._1}%.1f", f"${d2._2}%.1f",
        f"${d2._3}%.1f", f"${d2._4}%.1f", f"${d2._5}%.1f").mkString("\t"))
    })
    writer.close()

    saveSlides("slides" + timeStr)
  }

  def saveSlides(filename: String): Unit = {
    val writer = new PrintWriter(filename)
    slides.foreach(s => {
      writer.print(s.startTime + "\t")
      writer.print(s.initialPrice + "\t")
      writer.println(s.result.mkString("\t"))
    })
    writer.close()
  }

  /**
    * ピーク位置を保存。保存する時は時刻の新しい方からの、かつ1から始まるインデックスに変換する
    */
  def savePeaks(): Unit = {
    val len = metrics.length
    val uw = new PrintWriter("upeaks")
    upeaks.foreach(t => uw.println((len - t._1) + "\t" + (len - t._2)))
    uw.close()

    val lw = new PrintWriter("lpeaks")
    lpeaks.foreach(t => lw.println((len - t._1) + "\t" + (len - t._2)))
    lw.close()
  }


  /**
    * リロードボタンを押してもなぜか更新されない状態に陥ることが時々ある。エラーも例外も発生しない
    * 対策として、この状態になったらドライバーをクローズしてログインからやりなおす
    */
  def handleStall(): Unit = {
    val now = LocalDateTime.now()
    val prev = now.minusSeconds(60)

    // 60秒以上データが止まったらストールとみなす。
    // ただしエラーでログインに時間がかかった場合を考慮し、ログイン時刻の方がデータより新しいならストールとみなさない
    if (data.nonEmpty && data.head.time.compareTo(prev) < 0 && SBIFutureHandler.loginTime.compareTo(data.head.time) < 0) {

      // ただし、15:10-16:30と、05:25-08:45の間はストールとみなさない
      val nowTime = now.toLocalTime
      if ((nowTime.compareTo(LocalTime.of(8, 45)) > 0 && nowTime.compareTo(LocalTime.of(15, 10)) < 0) ||
        nowTime.compareTo(LocalTime.of(16, 30)) > 0 ||
        nowTime.compareTo(LocalTime.of(5, 25)) < 0) {
        StockLogger.writeMessage("Stall detected. attempt to re-login")
        SBIFutureHandler.close()
        SBIFutureHandler.login()
      }

    }
  }

  /**
    * データファイルを読み込み、セッションを再現する。データは先頭の方が新しいので、逆順で処理する
    */
  def replay(src: Path): Unit = {
    replayMode = true
    val ps = new ListBuffer[Price]

    object Proc extends Consumer[String] {
      override def accept(t: String): Unit = {
        val l = t.split("\t")
        val p = new Price(LocalDateTime.parse(l(0)), l(1).toDouble, Some(l(2).toInt))
        ps += p
      }
    }

    val strm = Files.lines(src)
    strm.forEach(Proc)
    strm.close()
    ps.reverse.foreach(add)
  }
}
