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
  var slidingMetrics = List.empty[Metrics]
  var slides = List.empty[SlidingWindow]
  var replayMode = false
  var peaks = List.empty[(Int, Int)] // 直近のピーク位置, ピークをピークと認識した位置。Listの末尾からのインデックスなので要注意
  var upeaks = List.empty[(Int, Int)] // 上に凸なピーク
  var lpeaks = List.empty[(Int, Int)] // 下に凸なピーク
  var peakClosing = false // 次のピークが近づいている
  var peakDirection = 0.0 // 現在のピークに対する次のピークの符号
  var peakCandidatePos = 0
  var atPeakDetectedPrice = 0.0 // ピーク検出時点の価格
  private[trader] var prevRange = (0.0, 0.0)
  private[trader] var currentRange = (0.0, 0.0)
  private[trader] var newRange = false
  private var priceOccurrence = scala.collection.mutable.Map.empty[Double, LocalDateTime]
  private var currentOccurrence = -1.0 // 最新の登録価格値

  def reset(): Unit = {
    data = List.empty[Price]
    SlidingWindow.reset()
    Metrics.reset()
    Haken.reset()
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
    priceOccurrence = scala.collection.mutable.Map.empty[Double, LocalDateTime]
    currentOccurrence = -1.0
  }

  def add(p: Price): Boolean = {
    if (!replayMode) handleStall()

    val amt = if (data.length <= 1) 1 else data.head.amt - data(1).amt
    // 出来高が前回と同じなら記録しない
    if (data.nonEmpty && p.amt == data.head.amt) false
    else {
      data = p :: data
      SlidingWindow.add(data)
      Metrics.add(data)
      Haken.add(Metrics.metrics)
      StrategyEvaluator.add(p)
      true
    }
  }

  /**
    * 直近2分間のm320, m1280, m2560の変化率を返す
   * @deprecated
    */
  private[trader] def maRate = {
    val t0 = data.head.time.minusSeconds(120)
    val at = data.count(p => p.time.compareTo(t0) >= 0)
    val m = Metrics.metrics
    val m0 = m.head
    if (at > 0) {
      val m1 = m(at - 1)
      (m0.m5 - m1.m5, m0.m20 - m1.m20, m0.m40 - m1.m40)
    } else (0.0, 0.0, 0.0)
  }

  /**
   * 直近n項目の移動平均の変化率を返す
   */
  private[trader] def maDiff(m: List[Metrics] = Metrics.metrics, n: Int = 20): (Double, Double, Double, Double) = {
    if (m.length < n) (0.0, 0.0, 0.0, 0.0)
    else {
      val m0 = m.head
      val m1 = m(n - 1)
      (m0.m5 - m1.m5, m0.m10 - m1.m10, m0.m20 - m1.m20, m0.m40 - m1.m40)
    }
  }

  private[trader] def amtDiff(d: List[Price] = data, n: Int = 20): Int = {
    val diff = if (d.length < n) (d.head.amt - d.last.amt).toDouble
    else d.head.amt - d(n - 1).amt
    val time = if (d.length < n) d.last.time.until(d.head.time, ChronoUnit.MILLIS)
    else d(n - 1).time.until(d.head.time, ChronoUnit.MILLIS)
    (diff / time * 1000.0).round.toInt
  }

  /**
    * 価格の出現時刻を登録し、
    * 価格が以前にも出現していれば今の時刻とのインターバルを返す。以前に出現していなければLongの最大値を返す
   * 価格が前回の価格から飛び離れている場合、間の価格も今回と同じ時刻に出現したとみなして登録する
   *
    */
  def registerPrice(price: Double, time: LocalDateTime = LocalDateTime.now()): Long = {
    val prev = priceOccurrence.put(price, time)
    if (currentOccurrence > 0 && (price - currentOccurrence).abs > 5.0) {
      val intp = price.toInt
      val intc = currentOccurrence.toInt
      val r = if (intp > intc) (intc until intp by 5).tail else (intp until intc by 5).tail
      r.foreach(p => priceOccurrence.put(p.toDouble, time))
    }

    currentOccurrence = price

    prev match  {
      case Some(s) => s.until(time, ChronoUnit.SECONDS)
      case None => Long.MaxValue
    }
  }

  /**
    * ピークを検出する。前回ピークよりgap以上離れているピークを検出感度sensで検出する
    * ピークを認識したらtrueを返す
    */
  def detectPeak(gap: Double = 5, sens: Double = 0.8): Boolean = {
    var res = false
    val m = Metrics.metrics
    val peakval = m(m.length - peaks.head._1 - 1).m5
    val currentval = m.head.m5
    val diff = currentval - peakval

    if (Math.abs(diff) > gap) {
      if (peakClosing) {
        val peakCandidateVal = m(m.length - peakCandidatePos - 1).m5
        val positiveDiff = (currentval - peakCandidateVal) * peakDirection
        if (positiveDiff > 0) {
          peakCandidatePos = m.length - 1
        } else if (positiveDiff < -sens) { // peak detected
          //          println(peakDirection + "peak " + peakCandidatePos + " detected at " + (metrics.length - 1))
          peaks = (peakCandidatePos, m.length - 1) :: peaks
          res = true
          peakClosing = false
        }
      } else {
        peakClosing = true
        peakDirection = Math.signum(diff)
        peakCandidatePos = m.length - 1
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
      val m = Metrics.metrics
      val p1 = m.length - peaks.head._1 - 1
      val p2 = m.length - peaks(1)._1 - 1
      val t1 = data(p1).time
      val t2 = data(p2).time
      val v1 = m(p1).m5
      val v2 = m(p2).m5
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
      val m = Metrics.metrics
      peaks.length match {
        case 0 => Double.NaN
        case 1 => m(m.length - peaks.head._1 - 1).m5
        case _ => m(m.length - peaks(pos)._1 - 1).m5
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

  def save(timeStr: String = legalTimeStr): Unit = {
    val writer = new PrintWriter("techs" + timeStr)

    data.zip(Metrics.metrics).foreach(d => {
      val d1 = d._1
      val d2 = d._2
      writer.println(List(d1.time, d1.price, d1.askPrice, d1.amt,
        f"${d2.amtrate}%.1f", f"${d2.m5}%.1f", f"${d2.m10}%.1f", f"${d2.m20}%.1f", f"${d2.m40}%.1f").mkString("\t"))
    })
    writer.close()

    saveSlides("slides" + timeStr)
  }

  def saveSlides(filename: String): Unit = {
    val writer = new PrintWriter(filename)
    SlidingWindow.slides.foreach(s => {
      val p = s.data.head
      writer.print(p.time + "\t")
      writer.print(p.askPrice + "\t")
      writer.println(s.result.mkString("\t"))
    })
    writer.close()
  }

  /**
    * ピーク位置を保存。保存する時は時刻の新しい方からの、かつ1から始まるインデックスに変換する
    */
  def savePeaks(): Unit = {
    val len = Metrics.metrics.length
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
    * 古いデータを読ませた時は売り気配値が無いことを識別して、売り気配値に0.0をセットする
    */
  def replay(src: Path): Unit = {
    replayMode = true
    val ps = new ListBuffer[Price]

    object Proc extends Consumer[String] {
      override def accept(t: String): Unit = {
        val l = t.split("\t")
        val time = LocalDateTime.parse(l(0))
        val price = l(1).toDouble
        var askPrice = 0.0
        var amt = 0
        try {
          amt = l(2).toInt
        } catch {
          case e: NumberFormatException =>
            askPrice = l(2).toDouble
            amt = l(3).toInt
        }
        ps += new Price(time, price, askPrice, amt)
      }
    }

    val strm = Files.lines(src)
    strm.forEach(Proc)
    strm.close()
    ps.reverse.foreach(add)
  }
}
