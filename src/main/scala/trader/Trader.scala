package trader

import java.time.LocalTime

import scala.io.Source

object Trader {
  val sp = SessionProcessor

  def main(args: Array[String]): Unit = {
/*
    sp.login()
    sp.initStocks()
    sp.buyClosing(0, 100)
    sp.updatePrices()
    sp.close()
    System.exit(0)
*/

    sp.lint
    sp.capitalAmt = args(0).toDouble
    println(sp.capitalAmt)

    while (LocalTime.now().compareTo(LocalTime.of(8, 57)) < 0) {
      Thread.sleep(60000)
    }

    sp.login()
    sp.initStocks()

    while (LocalTime.now().compareTo(LocalTime.of(9, 0)) < 0) {
      Thread.sleep(1000)
    }

    while (LocalTime.now().compareTo(LocalTime.of(9,10)) < 0) {
      sp.updatePrices()
      //sp.trade()
    }

    sp.setOpenPrices()

    while (LocalTime.now().compareTo(LocalTime.of(9,50)) < 0) {
      sp.updatePrices()
      //sp.trade()
    }
    sp.close()
    sp.login()
    while (LocalTime.now().compareTo(LocalTime.of(10,40)) < 0) {
      sp.updatePrices()
      //sp.trade()
    }
    sp.close()
    sp.login()
    while (LocalTime.now().compareTo(LocalTime.of(11, 30)) < 0) {
      sp.updatePrices()
      //sp.trade()
    }

    sp.close()
    for (s <- sp.stocks) {
      s.writeData()
    }


    while (LocalTime.now().compareTo(LocalTime.of(12, 27)) < 0) {
      Thread.sleep(60000)
    }

    sp.login()

    while (LocalTime.now().compareTo(LocalTime.of(12, 30)) < 0) {
      Thread.sleep(1000)
    }

    while (LocalTime.now().compareTo(LocalTime.of(13,20)) < 0) {
      sp.updatePrices()
      //sp.trade()
    }
    sp.close()
    sp.login()
    while (LocalTime.now().compareTo(LocalTime.of(14,10)) < 0) {
      sp.updatePrices()
      //sp.trade()
    }
    sp.close()
    sp.login()

    while (LocalTime.now().compareTo(LocalTime.of(14, 57, 0)) < 0) {
      sp.updatePrices()
      //sp.trade()
    }

    sp.tradeClosing()

    while (LocalTime.now().compareTo(LocalTime.of(15, 0, 30)) < 0) {
      sp.updatePrices()
    }

    for (s <- sp.stocks) {
      s.writeData()
    }

    sp.savePrices()
    sp.close()
    while (LocalTime.now().compareTo(LocalTime.of(17, 0)) < 0) {
      Thread.sleep(60000)
    }

    sp.login()
    sp.updatePrices()
    sp.orderSellOpening()

    StockLogger.close()
    sp.close()
  }

  def stockTest(fileName: String) = {
    val s = Source.fromFile(fileName)
    val it = s.getLines()
    it.next() // ヘッダをスキップ
    val p = for (l <- it) yield {
      val t = l.split('\t')
      (LocalTime.parse(t(0)), t(1).toDouble)
    }
    val pr = p.toList.reverse
    val stock = new Stock(0, pr.head._2)
    for (i <- pr.tail) {
      val j = stock.add(i._1, i._2)
      if (j == Judgement.SELL) {
        println(j, i)
        stock.volume = 0
      }
      if (j == Judgement.BUY) {
        println(j, i)
        stock.volume = stock.volumeunit
        stock.acquiredPrice = i._2
        stock.maxHolding = i._2
      }
    }
    println("done!")
  }
}
