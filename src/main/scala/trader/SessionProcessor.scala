package trader

import java.io.{File, PrintWriter}
import java.time.LocalTime

import scala.collection.JavaConverters._
import org.openqa.selenium.chrome.{ChromeDriver, ChromeOptions}
import org.openqa.selenium.support.ui.{ExpectedConditions, Select, WebDriverWait}
import org.openqa.selenium.{By, WebDriver, WebElement}

object SessionProcessor {
  var buyHolding = Seq.empty[WebElement]
  var buyAll: Seq[WebElement] =_
  var sellHolding =  Seq.empty[WebElement]
  var sellAll: Seq[WebElement] =_
  var codesHolding =  Seq.empty[WebElement]
  var codesAll: Seq[WebElement] =_
  var volumeHolding = Seq.empty[WebElement]
  var currentAll: Seq[WebElement] =_
  var acquiredHolding = Seq.empty[WebElement]
  var noHoldingStocks = false // 保有銘柄が無いかどうか
  var entryFull = false // 保有銘柄と一般預かり合わせて50以上か。50を超えるとポートフォリオが2ページにわたるため、これ以上買い注文を入れない
  var capitalAmt = 0.0 // 資金額。買付余力+保有銘柄の取得総額
  var driver: WebDriver =_

  var stocks: Array[Stock] =_
  type Judgement = trader.Judgement.Value
  var judgement: Seq[Judgement] =_

  def genDriver() = {
    val options = new ChromeOptions()
    options.addArguments("--headless")
    driver = new ChromeDriver(options)

/*
    val firefoxBinary = new FirefoxBinary
    firefoxBinary.addCommandLineOptions("--headless")
    System.setProperty("webdriver.gecko.driver", "C:/geckodriver/geckodriver.exe")
    val firefoxOptions = new FirefoxOptions
    firefoxOptions.setBinary(firefoxBinary)
    driver = new FirefoxDriver(firefoxOptions)
    println(driver)
*/

/*
    val driver: WebDriver = new HtmlUnitDriver(true) {

      import com.gargoylesoftware.htmlunit.BrowserVersion
      import com.gargoylesoftware.htmlunit.WebClient

      override protected def newWebClient(version: BrowserVersion): WebClient = {
        val webClient = super.newWebClient(version)
        webClient.getOptions.setThrowExceptionOnScriptError(false)
        webClient
      }
    }
*/
  }

  def login() = {
    genDriver()
    StockLogger.writeMessage("attempt to login")
    driver.get("https://www.sbisec.co.jp/")
    driver.findElement(By.name("user_id")).sendKeys(Settings.id)
    driver.findElement(By.name("user_password")).sendKeys(Settings.pwd)
    driver.findElement(By.name("ACT_login")).click()
    println(driver.getTitle)
    driver.findElement(By.xpath("//*[@id=\"link02M\"]/ul/li[1]/a")).click()
    //new WebDriverWait(driver, 10).until(
    //  ExpectedConditions.elementToBeClickable(By.className("portfolio"))).click()
    StockLogger.writeMessage("login done")
  }

  def close() = {
    try {
      driver.close()
    } catch { // まさかclose()が例外を投げることがあるとは…
      case e: Exception => {
        StockLogger.writeMessage(e.getMessage)
      }
    }
  }

  /**
    * stocksデータを作成する
    */
  def initStocks() = {
    refreshAllElements()
    val codes = codesAll.map(_.getText.take(4).toInt)
    val prices = currentAll.map(x => toDouble0(x.getText.replace(",", "")))
    stocks = codes.zip(prices).map(t => new Stock(t._1, t._2)).toArray
    for (s <- stocks.zipWithIndex) s._1.portfolionum = s._2
    setPrices()
    updatePrices()
  }

  def lint = setPrices(lint = true)

  def setPrices(lint: Boolean = false) = {
    val sc = new java.util.Scanner(new File("prices"))
    while (sc.hasNext) {
      val code  = sc.nextInt()
      println(code)
      val open = sc.nextDouble()
      val close = sc.nextDouble()
      val low = sc.nextDouble()
      val high = sc.nextDouble()
      val tickUnit = sc.nextDouble()
      val volumeUnit = sc.nextInt()
      val direction = sc.next()
      println(open, close, low, high, tickUnit, volumeUnit, direction)
      if (!lint) {
        for (s <- stocks if s.code == code) {
          s.prevOpen = open
          s.previous = close
          s.prevLow = low
          s.prevHigh = high
          s.tickUnit = tickUnit
          s.volumeunit = volumeUnit
          if (direction == "no") s.noHoldIfNegative = true
          if (direction == "exit") s.terminate = true
        }
      }
    }
    sc.close()
  }

  def savePrices() = {
    val writer = new PrintWriter("prices")
    for (s <- stocks) {
      val direction = {
        if (s.terminate) "exit"
        else if (s.noHoldIfNegative) "no"
        else "Hold"
      }
      writer.println(s.code +"\t"+ s.open +"\t"+ s.close +"\t"+ s.low +"\t"+ s.high +"\t"+
        s.tickUnit +"\t"+ s.volumeunit +"\t"+ direction)
    }
    writer.close()
  }

  /**
    * ポートフォリオ画面のエレメントを更新する
    * 保有銘柄がある場合、ポートフォリオ画面には保有銘柄と登録銘柄の2つのテーブルが表示されるが、
    * 保有銘柄が無ければ登録銘柄テーブルだけになる。
    * そのため2つ目のテーブルが存在しなければ、保有銘柄が無く登録銘柄のみと判断される
    */
  def refreshAllElements(): Unit = {
    val firstTablePath = "/html/body/div[3]/div/table/tbody/tr/td/table[4]/tbody/tr[2]/td/table/tbody"
    val secondTablePath = "/html/body/div[3]/div/table/tbody/tr/td/table[4]/tbody/tr[6]/td/table/tbody"
    try {
      new WebDriverWait(driver, 8).until(ExpectedConditions.presenceOfElementLocated(
        By.xpath(firstTablePath)))

      try {
        val secondTable = driver.findElement(By.xpath(secondTablePath))
        noHoldingStocks = false
      } catch {
        case e: org.openqa.selenium.NoSuchElementException => {
          if (!noHoldingStocks) StockLogger.writeMessage("no holding stocks")
          noHoldingStocks = true
        }
      }

      val tAllPath = if (noHoldingStocks) firstTablePath else secondTablePath

      val tAll = driver.findElement(By.xpath(tAllPath))
      val rowsAll = tAll.findElements(By.xpath("./tr[position() != 1]")).asScala
      buyAll = rowsAll.map(_.findElement(By.xpath("./td[1]/a[1]")))
      sellAll = rowsAll.map(_.findElement(By.xpath("./td[1]/a[2]")))
      codesAll = rowsAll.map(_.findElement(By.xpath("./td[2]")))
      currentAll = rowsAll.map(_.findElement(By.xpath("./td[6]")))

      buyHolding = Seq.empty[WebElement]
      sellHolding = Seq.empty[WebElement]
      codesHolding = Seq.empty[WebElement]
      volumeHolding = Seq.empty[WebElement]
      acquiredHolding = Seq.empty[WebElement]

      if (!noHoldingStocks) {
        val tHolding = driver.findElement(By.xpath(firstTablePath))
        val rowsHolding = tHolding.findElements(By.xpath("./tr[position() != 1]")).asScala
        buyHolding = rowsHolding.map(_.findElement(By.xpath("./td[1]/a[1]")))
        sellHolding = rowsHolding.map(_.findElement(By.xpath("./td[1]/a[2]")))
        codesHolding = rowsHolding.map(_.findElement(By.xpath("./td[2]")))
        volumeHolding = rowsHolding.map(_.findElement(By.xpath("./td[4]")))
        acquiredHolding = rowsHolding.map(_.findElement(By.xpath("./td[5]")))
      }

      if (codesAll.size + codesHolding.size >= 50) {
        if (!entryFull) StockLogger.writeMessage("entry full!")
        entryFull = true
      } else {
        if (entryFull) StockLogger.writeMessage("room appeared")
        entryFull = false
      }

    } catch {
      case e: org.openqa.selenium.TimeoutException => {
        StockLogger.writeMessage(e.getMessage)
        close()
        login()
        refreshAllElements()
      }
    }
}

  def refreshScreen() = {
    try {
      driver.navigate().refresh()
    } catch {
      case e: Exception => {
        println(e.getMessage)
        StockLogger.writeMessage(e.getMessage)
        close()
        login()
        refreshAllElements()
      }
    }
  }

  /**
    * StringからDoubleへの変換。変換できないときは0を返す
    */
  def toDouble0(s: String): Double = {
    var r = 0.0
    try {
      r = s.toDouble
    } catch {
      case _: NumberFormatException =>
    }
    r
  }

  def updatePrices() = {
    refreshScreen()
    refreshAllElements()
    val at = LocalTime.now

    val codes = codesHolding.map(_.getText.take(4).toInt)
    val acquirePrices = acquiredHolding.map(x => toDouble0(x.getText.replace(",", "")))
    val volumes = volumeHolding.map(_.getText.replace(",", "").toInt)
    for (t: (Int, Double, Int) <- (codes, acquirePrices, volumes).zipped) {
      for (s <- stocks) {
        if (s.code == t._1) {
          s.acquiredPrice = t._2
          s.volume = t._3
        }
      }
    }
    // 保有株式リストから消えている銘柄の数量と取得価格を0にする
    for (s <- stocks) {
      if (!(codes contains s.code)) {
        s.volume = 0
        s.acquiredPrice = 0.0
      }
    }
    val currentPrices = currentAll.map(x => toDouble0(x.getText.replace(",", "")))
    judgement = (for (sc: (Stock, Double) <- (stocks, currentPrices).zipped) yield sc._1.add(at, sc._2)).toSeq
    println(at)
  }

  /**
    * 大引けでの売買を実施する
    */
  def tradeClosing() = {
    val judgementClosing = for (s <- stocks) yield {
      s.closing = true
      s.judgeClosing
    }
    val zipped = judgementClosing.zipWithIndex
    val purchaseAmt = zipped.filter(z => z._1 == Judgement.BUY).foldLeft(0.0)((acc, z) =>
      acc + stocks(z._2).price * stocks(z._2).volumeunit)
    var leverage = 1
    if (purchaseAmt <= 0.0) StockLogger.writeMessage("no stocks purchased.")
    else {
      leverage = (capitalAmt / purchaseAmt * 0.97).toInt
      StockLogger.writeMessage("total purchase amount = " + purchaseAmt)
      StockLogger.writeMessage("capital = " + capitalAmt)
      StockLogger.writeMessage("leverage = " + leverage)
    }
    if (leverage < 1) leverage = 1 // for assurance
    for (j <- zipped.reverse) { // 降順で売買する
      val stock = stocks(j._2)

      if (stock.volume > 0 && j._1 == Judgement.SELL) sell(j._2, stock.volume)
      else  if (stock.volumeunit > 0 && stock.volume == 0 && j._1 == Judgement.BUY) buyClosing(j._2, stock.volumeunit)
    }
  }

  /**
    * 売買を実施する
    */
  def trade() = {
    for (j <- judgement.zipWithIndex) {
      val stock = stocks(j._2)

      if (j._1 == Judgement.BUY) {
        if (stock.volume > 0) StockLogger.writeMessage("trade error: " + stock.code + "volume > 0")
        else if (stock.volumeunit <= 0) StockLogger.writeMessage("trade error: " + stock.code + " volume unit is still not set")
        else {
          if (stock.buyUnspecified) buy(j._2, stock.volumeunit)
          else if (stock.buyPrice <= 0.0) StockLogger.writeMessage("trade error: " + stock.code + " buy price unspecified")
          else buySpecify(j._2, stock.volumeunit, stock.buyPrice, stock.buyThreshold)
        }
      }
      if (j._1 == Judgement.SELL) {
        if (stock.volume == 0) StockLogger.writeMessage("trade error: " + stock.code + "volume = 0")
        else if (!stock.openDone) sell(j._2, stock.volume)
        else {
          if (stock.sellPrice <= 0.0) StockLogger.writeMessage("trade error: " + stock.code + " sell price unspecified")
          sellSpecify(j._2, stock.volume, stock.sellPrice, stock.sellThreshold)
        }
      }
    }
  }

  /**
    * 寄り付きで全株式の売却を発注する
    */
  def orderSellOpening() = {
    for (s <- stocks) {
      if (s.volume > 0) sellOpening(s.portfolionum, s.volume)
    }
  }

  /**
    * 始値を取得して設定する
    * 取得できなかった場合は後でStockに取得をリクエストさせる
    */
  def setOpenPrices() = {
    for (s <- stocks zipWithIndex) {
      val open = getOpenPrice(s._2)
      if (open > 0.0) {
        s._1.open = open
        s._1.actualOpenPrice = true
      }
      s._1.setOpenPricesDone = true
    }
  }

  /**
    * 指定された銘柄の始値を取得する。取得できない場合は0.0を返す
    */
  def getOpenPrice(portfolionum: Int): Double = {
    buyAll(portfolionum).click()
    var open = 0.0
    var trytimes = 0
    while (open == 0.0 && trytimes < 8) {
      try {
        val openStr = driver.findElement(By.id("MTB0_2")).getText.replace(",", "")
        println(openStr)
        open = toDouble0(openStr.substring(0, openStr.size - 8))
      } catch {
        case e: Exception => {
          StockLogger.writeMessage(e.getMessage)
          close()
          login()
          refreshAllElements()
        }
      }
      if (open == 0.0) Thread.sleep(500)
      trytimes += 1
    }
    driver.findElement(By.xpath("/html/body/div[1]/div[1]/div[2]/div/ul/li[1]/a/img")).click()
    refreshAllElements()
    if (open > 0.0) {
      StockLogger.writeMessage("open price for " + stocks(portfolionum).code + " = " + open)
    } else StockLogger.writeMessage("open price for " + stocks(portfolionum).code + " is still unknown")
    open
  }

  /**
    * 購入を逆指値指定する
    */
  def buySpecify(i: Int, vol: Int, price: Int, threshold: Int) = {
    val sl = StockLogger
    sl.writeMessage("buying " + i + " " + vol)
    val href = buyAll(i).getAttribute("href")
    // println(href)
    buyAll(i).click()
    println(driver.getTitle)
    // 数量入力
    driver.findElement(By.name("input_quantity")).sendKeys(vol.toString)
    // ラジオボタンで逆指値を選択
    val in_sasinari_kbn = driver.findElements(By.name("in_sasinari_kbn")).asScala
    in_sasinari_kbn(2).click()
    // 逆指値入力
    new WebDriverWait(driver, 2).until(ExpectedConditions.presenceOfElementLocated(
      By.id("input_trigger_price")))
    driver.findElement(By.id("input_trigger_price")).sendKeys(threshold.toString)
    driver.findElement(By.id("gsn_input_price")).sendKeys(price.toString)
    // パスワード入力
    driver.findElement(By.name("trade_pwd")).sendKeys(Settings.pwd2)
    // 発注確認画面省略
    driver.findElement(By.name("skip_estimate")).click()
    // 発注。注文受付画面へ遷移
    new WebDriverWait(driver, 2).until(ExpectedConditions.presenceOfElementLocated(
      By.id("botton2")))
    driver.findElement(By.id("botton2")).click()
    // ポートフォリオ画面へ遷移
    driver.findElement(By.xpath("/html/body/div[1]/div[1]/div[2]/div/ul/li[1]/a/img")).click()

    sl.writeMessage(i + " buy order specified at " + price)
    sl.writeMessage(driver.getTitle)
    refreshAllElements()
  }
  /**
    * 購入する
    */
  def buy(i: Int, vol: Int) = {
    val sl = StockLogger
    sl.writeMessage("buying " + i + " " + vol)
    val href = buyAll(i).getAttribute("href")
    // println(href)
    buyAll(i).click()
    println(driver.getTitle)
    // 数量入力
    driver.findElement(By.name("input_quantity")).sendKeys(vol.toString)
    // ラジオボタンで成行を選択
    val in_sasinari_kbn = driver.findElements(By.name("in_sasinari_kbn")).asScala
    in_sasinari_kbn(1).click()
    // パスワード入力
    driver.findElement(By.name("trade_pwd")).sendKeys(Settings.pwd2)
    // 発注確認画面省略
    driver.findElement(By.name("skip_estimate")).click()
    // 発注。注文受付画面へ遷移
    new WebDriverWait(driver, 2).until(ExpectedConditions.presenceOfElementLocated(
      By.id("botton2")))
    driver.findElement(By.id("botton2")).click()
    // ポートフォリオ画面へ遷移
    driver.findElement(By.xpath("/html/body/div[1]/div[1]/div[2]/div/ul/li[1]/a/img")).click()

    sl.writeMessage("buy order done")
    sl.writeMessage(driver.getTitle)
    refreshAllElements()
  }

  /**
    * 寄り付きで売却する
    */
  def sellOpening(i: Int, vol: Int) = {
    val sl = StockLogger
    sl.writeMessage("selling " + i + " " + vol)
    val href = sellAll(i).getAttribute("href")
    // println(href)
    sellAll(i).click()
    println(driver.getTitle)
    // 数量入力
    driver.findElement(By.name("input_quantity")).sendKeys(vol.toString)
    // SOR指定を解除
    val sor = driver.findElement(By.id("sor_check"))
    if (sor.isSelected) sor.click()
    // セレクト要素で寄付を選択
    val select = new Select(driver.findElement(By.name("nariyuki_condition")))
    select.selectByIndex(1)
    // パスワード入力
    driver.findElement(By.name("trade_pwd")).sendKeys(Settings.pwd2)
    // 発注確認画面省略
    driver.findElement(By.name("skip_estimate")).click()
    // 発注。注文受付画面へ遷移
    new WebDriverWait(driver, 2).until(ExpectedConditions.presenceOfElementLocated(
      By.id("botton2")))
    driver.findElement(By.id("botton2")).click()
    // ポートフォリオ画面へ遷移
    driver.findElement(By.xpath("/html/body/div[1]/div[1]/div[2]/div/ul/li[1]/a/img")).click()

    sl.writeMessage("sell oder done")
    sl.writeMessage(driver.getTitle)

    refreshAllElements()
  }

  /**
    * 引け成りで購入する
    */
  def buyClosing(i: Int, vol: Int) = {
    val sl = StockLogger
    sl.writeMessage("buying " + i + " " + vol)
    val href = buyAll(i).getAttribute("href")
    // println(href)
    buyAll(i).click()
    println(driver.getTitle)
    // 数量入力
    driver.findElement(By.name("input_quantity")).sendKeys(vol.toString)
    // SOR指定を解除
    val sor = driver.findElement(By.id("sor_check"))
    if (sor.isSelected) sor.click()
    // セレクト要素で引成を選択
    val select = new Select(driver.findElement(By.name("nariyuki_condition")))
    select.selectByIndex(2)

    // パスワード入力
    driver.findElement(By.name("trade_pwd")).sendKeys(Settings.pwd2)
    // 発注確認画面省略
    driver.findElement(By.name("skip_estimate")).click()
    // 発注。注文受付画面へ遷移
    new WebDriverWait(driver, 2).until(ExpectedConditions.presenceOfElementLocated(
      By.id("botton2")))
    driver.findElement(By.id("botton2")).click()
    // ポートフォリオ画面へ遷移
    driver.findElement(By.xpath("/html/body/div[1]/div[1]/div[2]/div/ul/li[1]/a/img")).click()

    sl.writeMessage("buy order done")
    sl.writeMessage(driver.getTitle)
    refreshAllElements()
  }

  /**
    * 売却を逆指値指定する
    */
  def sellSpecify(i: Int, vol: Int, price: Int, threshold: Int) = {
    val sl = StockLogger
    sl.writeMessage("selling " + i + " " + vol)
    val href = sellAll(i).getAttribute("href")
    // println(href)
    sellAll(i).click()
    println(driver.getTitle)
    // 数量入力
    driver.findElement(By.name("input_quantity")).sendKeys(vol.toString)
    // ラジオボタンで逆指値を選択
    val in_sasinari_kbn = driver.findElements(By.name("in_sasinari_kbn")).asScala
    in_sasinari_kbn(2).click()
    // 逆指値入力
    new WebDriverWait(driver, 2).until(ExpectedConditions.presenceOfElementLocated(
      By.id("input_trigger_price")))
    driver.findElement(By.id("input_trigger_price")).sendKeys(threshold.toString)
    driver.findElement(By.id("gsn_input_price")).sendKeys(price.toString)
    // パスワード入力
    driver.findElement(By.name("trade_pwd")).sendKeys(Settings.pwd2)
    // 発注確認画面省略
    driver.findElement(By.name("skip_estimate")).click()
    // 発注。注文受付画面へ遷移
    new WebDriverWait(driver, 2).until(ExpectedConditions.presenceOfElementLocated(
      By.id("botton2")))
    driver.findElement(By.id("botton2")).click()
    // ポートフォリオ画面へ遷移
    driver.findElement(By.xpath("/html/body/div[1]/div[1]/div[2]/div/ul/li[1]/a/img")).click()

    sl.writeMessage(i + " sell order specified at " + price)
    sl.writeMessage(driver.getTitle)

    refreshAllElements()
  }
  /**
    * 売却する
    */
  def sell(i: Int, vol: Int) = {
    val sl = StockLogger
    sl.writeMessage("selling " + i + " " + vol)
    val href = sellAll(i).getAttribute("href")
    // println(href)
    sellAll(i).click()
    println(driver.getTitle)
    // 数量入力
    driver.findElement(By.name("input_quantity")).sendKeys(vol.toString)
    // ラジオボタンで成行を選択
    val in_sasinari_kbn = driver.findElements(By.name("in_sasinari_kbn")).asScala
    in_sasinari_kbn(1).click()
    // パスワード入力
    driver.findElement(By.name("trade_pwd")).sendKeys(Settings.pwd2)
    // 発注確認画面省略
    driver.findElement(By.name("skip_estimate")).click()
    // 発注。注文受付画面へ遷移
    new WebDriverWait(driver, 2).until(ExpectedConditions.presenceOfElementLocated(
      By.id("botton2")))
    driver.findElement(By.id("botton2")).click()
    // ポートフォリオ画面へ遷移
    driver.findElement(By.xpath("/html/body/div[1]/div[1]/div[2]/div/ul/li[1]/a/img")).click()

    sl.writeMessage("sell oder done")
    sl.writeMessage(driver.getTitle)

    refreshAllElements()
  }
}
