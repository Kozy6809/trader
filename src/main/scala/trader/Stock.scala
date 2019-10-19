package trader

import java.io._
import java.time._

import com.google.common.collect.MultimapBuilder.ListMultimapBuilder

import scala.collection.mutable.ListBuffer

/**
  * 銘柄の各種データを保持する
  * code 銘柄コード
  * prevPrice 前日終値
  */
class Stock(val code: Int, var previous: Double) {
  var price: Double =_
  private[this] var acquiredPriceValue: Double =_
  var maxHolding: Double = 0.0 // 株式取得後の最大価格。取得のたびにリセットされる
  var volumeunit: Int =_ // 売買数量単位。0なら始値から計算される
  var volume: Int =_ // 現在の保有数量。1回の取引ですべての数量が約定しなければ、volumeと一致しないケースが出てくる
  var incomplete: Boolean = false // 注文がまだ約定完了していないことを示す
  var currentOrder: Judgement = Judgement.STAY
  var openDone = true // open手続きが終了したらtrue
  var openPriceDone = false// priceがprevPriceから変化したらtrue
  var actualOpenPrice = false // openがwebから取得した正しい値か
  var setOpenPricesDone = false // SessionProcessor.setOpenPrices()が既に終了している。それでも始値が得られない場合がある
  var closing = false // 大引け直前であることを示す。
  var lossCount = 0 // 購入価格以下で売却した回数
  val ticks = new Ticks()
  var prevOpen: Double =_
  var prevLow: Double =_
  var prevHigh: Double =_
  var open: Double =_
  var close: Double =_
  var low: Double = Double.MaxValue
  var high: Double = 0.0
  var accLoss: Double =_ // 損失の累積値
  var overLoss = false // 損失が許容限度を超えたか
  var tickUnit: Double =_ // 呼び値。0の時は価格から計算される。銘柄によっては計算式による値と異なることがある
  var soldOnce = false // 閾値割れによる売りが発生したか
  var portfolionum: Int =_ // ポートフォリオ画面中の上から数えた番号(0始まり)
  var insensitiveTime: LocalTime =_ // 始値が決まってから売買を控える期間の期限
  var extendInsensitive = false // insensitive期間を延長したか
  var insensitiveDone = false // insensitive期間を終了したか
  var noHoldIfNegative = false // 陰線の場合保持しない設定か
  var sellPrice = 0 // 売却する場合の指値
  var sellThreshold = 0 // 売却時の逆指値。sellPriceより2ティック上
  var buyPrice = 0 // 購入する場合の指値
  var buyThreshold = 0 // 購入時の逆指値。buyPriceより2ティック下
  var buyUnspecified = false // 購入を成行で発注するか
  var terminate = false // openモードで売り抜けたら手仕舞いにするか

  type Judgement = trader.Judgement.Value

  def acquiredPrice = acquiredPriceValue
  def acquiredPrice_=(p: Double): Unit = {
    acquiredPriceValue = p
    if (p > maxHolding) maxHolding = p
  }

  /**
    * 前日終値から値が動いたらそれを始値に設定し、売買単位を決定する
    */
  def checkOpenPrice() = {
    if (!openPriceDone && price != previous) {
      insensitiveTime = LocalTime.now().plusMinutes(5)
      println(code + " open price is " + price)
      open = price
      if (volumeunit == 0) volumeunit = (1000 / open).toInt * 100 + 100
      openPriceDone = true
    }

    if (openPriceDone && !actualOpenPrice && setOpenPricesDone) {
      val actual = SessionProcessor.getOpenPrice(portfolionum)
      if (actual > 0.0) {
        open = actual
        actualOpenPrice = true
      }
    }
  }

  /**
    * 大引けで株式を保持するかどうか判断する
    * 陰線でnoHoldIfNegativeなら保持しない
    * ギャップアップなら陰陽によらず買い、ギャップダウンなら陰陽によらず売り
    * 本日陰線なら保持しない。ただし終値が前日の値幅よりあまり下がっていなければ買い
    * 本日陽線で前日の範囲よりも十分上に来ていれば保持する
    * 前日陰線、本日陽線で本日始値が前日終値以上なら保持する
    */
  def judgeClosing: Judgement = {
    val sp = SessionProcessor
    if (terminate) Judgement.STAY
    else if (close == open) Judgement.STAY
    else if (close < open && noHoldIfNegative) Judgement.SELL
    else if (Math.min(open, close) >= Math.max(prevOpen, previous) && !sp.entryFull) Judgement.BUY
    else if (Math.max(open, close) <= Math.min(prevOpen, previous)) Judgement.SELL
    else if (close < open) {
      if ((Math.min(prevOpen, previous) - close) / (open - close) < 0.5) {
        if (sp.entryFull) Judgement.STAY else Judgement.BUY
      } else Judgement.SELL
    } else {
      if ((Math.max(previous, prevOpen) - open) / (close - open) < 0.5) {
        if (sp.entryFull) Judgement.STAY else Judgement.BUY
      } else if (previous < prevOpen && open >= previous && !sp.entryFull) Judgement.BUY
      else Judgement.SELL
    }
  }
  /**
    * 寄り付き時の売買判断。
    * 前日からholdしていて値動きがMAXIMALかDESCENDINGならSTAYでopen完了し、通常モードへ移行。さもなくばopen継続
    * 本日購入するなら9時5分までは様子見。ただし9時5分までに20ティック以上値上がりしたら購入
    * 9時5分を過ぎても値動きがMAXIMALかDESCENDINGならSTAYでopen継続。さもなくばSTAYでopen完了
    */
  def judgeOpen: Judgement = {
    val p = detectPole()
    val j =
    if (volume > 0) { // 保有株式がある場合
      if (p(1) == Trend.MAXIMAL || p(1) == Trend.DESCENDING) {
        Judgement.SELL
      } else Judgement.STAY
    } else { // 保有株が無い場合
      if (!openPriceDone) Judgement.STAY
      else {
        if (LocalTime.now().compareTo(insensitiveTime) >= 0) insensitiveDone = true
        if (LocalTime.now().compareTo(LocalTime.of(9, 15)) < 0) Judgement.STAY
        else {
          if (!insensitiveDone) Judgement.STAY
          else {
            if (!extendInsensitive) { // 不感期間を延長するか1度だけ判断する
              val exceed = (price - open * 1.015) / priceTick(price)
              if (exceed > 2.0) {
                insensitiveTime = LocalTime.now().plusSeconds(Math.round(exceed * 30.0).toLong)
                insensitiveDone = false
              }
              extendInsensitive = true
              Judgement.STAY
            } else {
              if (p(1) == Trend.MAXIMAL || p(1) == Trend.DESCENDING) Judgement.STAY
              else {
                openDone = true
                Judgement.STAY
              }
            }
          }
        }
      }
    }
    StockLogger.write(ticks.data.head._1, code, j, price)
    j
  }

  def judge: Judgement = {
    if (terminate) Judgement.STAY
    else {
      var j = if (volume > 0) judgeSell else judgeBuy
      if (closing) j = Judgement.STAY
      StockLogger.write(ticks.data.head._1, code, j, price)
      j
    }
  }

  /**
    * 価格に応じた呼び値を返す
    * @param price
    * @return
    */
  def priceTick(price: Double): Double = {
    if (tickUnit > 0.0) tickUnit
    else {
      if (price <= 3000.0) 1.0
      else if (price <= 10000.0) 5.0
      else if (price <= 30000.0) 10.0
      else if (price <= 50000.0) 50.0
      else if (price <= 300000.0) 100.0
      else if (price <= 500000.0) 500.0
      else if (price <= 3000000.0) 1000.0
      else if (price <= 5000000.0) 5000.0
      else if (price <= 30000000.0) 10000.0
      else if (price <= 50000000.0) 50000.0
      else 100000.0
    }
  }

  /**
    * 実数値を呼値で丸める
    * @return
    */
  def roundTick(p: Double): Double = {
    val tick = priceTick(p)
    Math.round(p / tick) * tick
  }

  def judgeSell: Judgement = {
    if (volume < volumeunit) Judgement.STAY
    else if (buyPrice <= 0.0) Judgement.STAY
    else if (sellPrice > 0.0) Judgement.STAY
    else {
      soldOnce = true
      sellPrice = roundTick(open - priceTick(open)).toInt
      sellThreshold = open.toInt
      Judgement.SELL
    }
/*
    if (price <= Math.min(open, open * 1.015 - priceTick(price) * 12)) {
      soldOnce = true
      Judgement.SELL
    } else Judgement.STAY
*/
  }

  /**
    * openモード終了後にopen価格より十分高い購入価格を逆指値指定する
    * 閾値以下でもほとんどの銘柄で逆指値指定するつもりだったが、それでは買付余力を食われてしまい、引けで資金不足になる。
    * そのため閾値を超えてから逆指値指定することにする。そのため逆指値だがすぐに執行される
    * openモード終了時点で既に価格が閾値より高かったら成行買いする
    * @return
    */
  def judgeBuy: Judgement = {
    Judgement.STAY
/*
    val p = detectPole()
    if (SessionProcessor.entryFull) Judgement.STAY
    else if (soldOnce) Judgement.STAY
    else if (buyPrice > 0.0) Judgement.STAY
    else if (!actualOpenPrice) Judgement.STAY
    else if (price < open) Judgement.STAY
    else if (p(1) == Trend.MAXIMAL || p(1) == Trend.DESCENDING) Judgement.STAY
    else {
      if (price > open * 1.015) {
        buyThreshold = roundTick(open * 1.015).toInt
        buyPrice = roundTick(buyThreshold + priceTick(buyThreshold)).toInt
        // buyUnspecified = if (price > buyPrice) true else false
        buyUnspecified = false
        Judgement.BUY
      } else Judgement.STAY
    }
*/
  }


  def checkCompletion() = {
    if (currentOrder == Judgement.SELL) {
      if (volume != 0) incomplete = true
      else {
        incomplete = false
        StockLogger.writeMessage(code + " sell completed. price = " + price)
      }
    }
    if (currentOrder == Judgement.BUY) {
      if (volume != volumeunit) incomplete= true
      else {
        incomplete = false
        maxHolding = 0.0
        StockLogger.writeMessage(code + " buy completed. price = " + acquiredPrice)
      }
    }
    if (!incomplete) currentOrder = Judgement.STAY
  }

  /**
    * 価格データを追加し、売買判断を返す
    * @param at
    * @param price
    * @return +1: 買い判断 -1: 売り判断 0: そのまま
    */
  def add(at: LocalTime, price: Double): Judgement = {
    checkCompletion()
    println(code +" price: " + price)
    var judgement = Judgement.STAY
    this.price = price
    close = price
    if (price < low) low = price
    if (price > high) high = price
    if (price > maxHolding) maxHolding = price
    checkOpenPrice()
    ticks.add(at, price)

    if (openPriceDone && ticks.data.size >= 3 && !incomplete) {
      judgement = if (!openDone) judgeOpen else judge
    } else judgement = Judgement.STAY
    if (incomplete) Judgement.STAY
    else {
      currentOrder = judgement
      judgement
    }
  }

  /**
    * 時系列データの書き出し
    */
  def writeData() = {
    println(code)
    val writer = new PrintWriter(new File(code.toString))
    writer.println("time\tprice\tma8\tma64")
    for (l <- ticks.data) {
      writer.print(l._1)
      writer.print("\t")
      writer.print(l._2)
      writer.print("\t")
      writer.print(l._3)
      writer.print("\t")
      writer.println(l._6)
    }
    writer.close()
  }

  def detectPole() = {
    val d = ticks.data.take(3)

    for (i <- 1 to 5) yield {
      val l = d.map(_.productElement(i).asInstanceOf[Double])
      judgeTrend(l)
    }
  }

  def detectCrossing(): Int = {
    val d = ticks.data.take(3)
    if (((d(0)._3 > d(0)._5 && d(2)._3 <= d(2)._5)) || (d(0)._3 >= d(0)._5 && d(2)._3 < d(2)._5)) 1
    else if (((d(0)._3 < d(0)._5 && d(2)._3 >= d(2)._5)) || (d(0)._3 <= d(0)._5 && d(2)._3 > d(2)._5)) -1
    else 0
  }

  def judgeTrend(l: Seq[Double]) = {
    l match {
      case l if (l(2) < l(1) && l(1) > l(0)) => Trend.MAXIMAL
      case l if (l(2) > l(1) && l(1) < l(0)) => Trend.MINIMAL
      case l if ((l(2) <= l(1) && l(1) < l(0)) || (l(2) < l(1) && l(1) <= l(0))) => Trend.ASCENDING
      case l if ((l(2) >= l(1) && l(1) > l(0)) || (l(2) > l(1) && l(1) >= l(0))) => Trend.DESCENDING
      case _ => Trend.FLAT    }
  }

  object Trend extends Enumeration(-2) {
    val DESCENDING, MINIMAL, FLAT, MAXIMAL, ASCENDING = Value
  }
  type Trend = Trend.Value

}

/**
  * 株価データを時系列で保持する
  * 時刻
  * 価格
  * 移動平均8
  * 移動平均16
  * 移動平均32
  */
class Ticks {
  var data = List.empty[(LocalTime, Double, Double, Double, Double, Double)]
  var hist = scala.collection.mutable.Map.empty[Double, Int] // 価格のヒストグラムデータ
  var spannedPrices = ListBuffer.empty[Double]
  var acc = 0.0
  var n = 0

  def add(at: LocalTime, price: Double) = {
    val ma8 = ma(price, 8)
    val ma16 = ma(price, 16)
    val ma32 = ma(price, 32)
    val ma64 = ma(price, 64)
    data = (at, price, ma8, ma16, ma32, ma64) :: data
    if (hist.contains(price)) hist(price) += 1
    else hist += (price -> 1)
    spannedPrices += price
    acc += price
    n += 1
  }

  /**
    * 価格の標準偏差から新しい閾値を返す
    * 平均値より閾値に近い側で、1σの位置の価格を返す
    */
  def sdThresh(currentThresh: Double, sell: Boolean): Double = {
    val mean = acc / n
    val sd = Math.sqrt(spannedPrices.map(x => (x - mean) * (x - mean)).sum / n)
    val scaled = (currentThresh - mean) / sd
    val res = mean + (if (sell) sd * -2 else sd * 2)
    //val res = if (Math.abs(scaled) > 1) currentThresh else if (scaled > 0) mean + sd * 1  else mean - sd * 1
    println("mean: " + mean + " sd: " + sd + " new thresh = " + res)
    spannedPrices = ListBuffer.empty[Double]
    acc = 0.0
    n = 0
    res
  }

  /**
    * 価格の度数分布から新しい閾値を返す
    * 最頻値より閾値に近い側で、最頻値の1/10以下の度数の価格を返す
    */
  def newThresh(currentThresh: Double): Double = {
    val modePrice = hist.keys.maxBy(hist(_))
    println("modePrice = " + modePrice + ", freq = " + hist(modePrice))
    val limit = hist(modePrice) / 10
    val candidates = hist.keys.filter(hist(_) <= limit)
    println(candidates)
    val res =
    if (candidates.size == 0) currentThresh
    else {
      if (modePrice >= currentThresh) {
        val qualified = candidates.filter(_ < currentThresh)
        if (qualified.size > 0) qualified.max else currentThresh
      }
      else {
        val qualified = candidates.filter(_ > currentThresh)
        if (qualified.size > 0) qualified.min else currentThresh
      }
    }
    hist = scala.collection.mutable.Map.empty[Double, Int]
    println("new threshold = " + res)
    res
  }

  def ma(price: Double, span: Int): Double = {
    val d = price :: data.take(span - 1).map(_._2)
    val sum =  d.foldLeft(0.0)((x, y) => x + y)
    sum / d.size
  }
}