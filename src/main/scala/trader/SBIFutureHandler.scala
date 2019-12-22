package trader

import java.time.{Duration, LocalDateTime, LocalTime}

import org.openqa.selenium.support.ui.Select
import org.openqa.selenium.{By, Dimension, WebDriver}
//import org.openqa.selenium.chrome.{ChromeDriver, ChromeOptions}
import org.openqa.selenium.firefox._
import org.openqa.selenium.interactions.Actions
import org.openqa.selenium.support.ui.{ExpectedConditions, WebDriverWait}

/**
  * SBI証券の先物サイトからデータをやりとりする
  *
  */
object SBIFutureHandler {
  var driver: WebDriver = _
  var loginTime: LocalDateTime = _

  /**
    * ChromeDriverはChromeバージョン75までしか対応していない。
    * Driver4.0.0が正式バージョンになれば使えるかも
    *
    * プライスボードのフレームをリサイズして表示させないと対応するHTMLファイルが
    * 読み込まれないので、ヘッドレスモードではうまくいかないかも知れない
    * →ヘッドレスで動作した!!q(^o^)p
    * →一度プライスボードボタンをクリックして表示させるようにしたら、その後はデフォルトで表示されるようになった
    * 時々余計な広告ポップアップが出るので、消えるまで待つ必要もある
    */
  def genDriver(): Unit = {
    //val options = new ChromeOptions()
    //options.addArguments("--headless")
    //driver = new ChromeDriver(options)


    val firefoxBinary = new FirefoxBinary
    firefoxBinary.addCommandLineOptions("--headless")
    System.setProperty("webdriver.gecko.driver", "C:/geckodriver/geckodriver.exe")
    val firefoxOptions = new FirefoxOptions
    firefoxOptions.setBinary(firefoxBinary)
    driver = new FirefoxDriver(firefoxOptions)
    println(driver)


  }

  def login(): Unit = {
    genDriver()
    StockLogger.writeMessage("attempt to login")
    // ログイン画面にアクセス
    driver.get("https://www.sbisec.co.jp/ETGate/?OutSide=on&_ControlID=WPLETsmR001Control&_DataStoreID=DSWPLETsmR001Control&sw_page=Future&cat1=home&cat2=none&getFlg=on")
    // ID入力
    driver.findElement(By.name("user_id")).sendKeys(Settings.id)
    // パスワード入力
    driver.findElement(By.name("user_password")).sendKeys(Settings.pwd)
    // ログインボタンクリック
    val login = driver.findElement(By.name("ACT_login"))
    val wait = new WebDriverWait(driver, Duration.ofSeconds(60))
    wait.until(ExpectedConditions.elementToBeClickable(login))
    // ポップアップでクリックボタンが押せない時、ポップアップが消えるのを待つ
    var done = false
    while (!done) {
      try {
        login.click()
        done = true
      } catch {
        case e: Exception =>
          StockLogger.writeMessage("login failed. retrying click.")
          Thread.sleep(1000)
      }
    }
    /**
      * "重要なお知らせ"がポストされると、それを読まないと取引画面が表示されない。しかも文書はポップアップで表示されるので、ウィンドウ遷移処理が必要
      * これが発生するのはログイン時だけではなく、取引画面のリロードを繰り返している最中にも生じる
      * "重要なお知らせ"のxpath:
      * /html/body/div[1]/table/tbody/tr/td[1]/table/tbody/tr[2]/td[2]/table/tbody/tr[3]/td/div/b
      * 最新メッセージのxpath:
      * /html/body/div[1]/table/tbody/tr/td[1]/table/tbody/tr[2]/td[2]/form/table[3]/tbody/tr/td/table/tbody/tr[2]/td[2]/table/tbody/tr[2]/td/a
      * 遷移後のウィンドウで、内容を理解した旨のボタンのxpath:
      * /html/body/div[1]/table[3]/tbody/tr[1]/td[2]/form/table[4]/tbody/tr/td/input[1]
      * xpathよりもname="ACT_estimate"を使う方が簡単
      * 同意ボタンをクリックすると文書の再読み込みが発生する
      * 遷移後のウィンドウを閉じるボタンのxpath:
      * /html/body/div[1]/table[3]/tbody/tr[1]/td[2]/form/table[6]/tbody/tr[4]/td/input
      * これもname="ACT_backViewInfoList"の方が楽
      * 閉じるボタンをクリックしても閉じない?要素を調査中だったから?
      * 元画面に戻っても重要なお知らせのままなので、ログインからやり直そうとすると、既にログイン済みなので取引画面が表示される
      */
    val element = wait.until(ExpectedConditions.visibilityOfElementLocated(By.name("main")))
    println(driver.getTitle)

    StockLogger.writeMessage("login done")
    loginTime = LocalDateTime.now()

    // プライスボードボタンをクリック
//    wait.until(ExpectedConditions.frameToBeAvailableAndSwitchToIt("menu"))
//    wait.until(ExpectedConditions.frameToBeAvailableAndSwitchToIt("menubody"))
//    wait.until(ExpectedConditions.elementToBeClickable(By.id("formCM0004:priceLink"))).click()
    // /html/frameset/frame[1] name=menu
    // /html/frameset/frame[2] name=menubody
    // //*[@id="formCM0004:priceLink"]
//    val action = new Actions(driver)
//    action.moveByOffset(100, 130).clickAndHold().moveByOffset(0, 250).release().perform()
    priceBoard()
    StockLogger.writeMessage("price board displayed")

    showContentsFrame(0)

    StockLogger.writeMessage("new SELL screen displayed")
  }

  def priceBoard(): Unit = {
    val wait = new WebDriverWait(driver, Duration.ofSeconds(60))

    driver.switchTo().defaultContent()
    val mainFrame = wait.until(ExpectedConditions.frameToBeAvailableAndSwitchToIt("main"))
    val priceFrame = wait.until(ExpectedConditions.frameToBeAvailableAndSwitchToIt("price"))
    val firstRow = wait.until(ExpectedConditions.visibilityOfElementLocated(By.xpath("//*[@id='datagrid-row-r7-2-0']")))

  }

  /**
    *
    * @param buttonNum 0: 新規売 1: 新規買 3: 決済売 4: 決済買
    * @return
    */
  def showContentsFrame(buttonNum: Int): Unit = {
    priceBoard()
    val wait = new WebDriverWait(driver, Duration.ofSeconds(60))
    val firstRow = wait.until(ExpectedConditions.visibilityOfElementLocated(By.xpath("//*[@id='datagrid-row-r7-2-0']")))
    val tradeButtons = firstRow.findElements(By.xpath("td[15]/div/button"))
    tradeButtons.get(buttonNum).click()
    driver.switchTo().parentFrame()
    val contentsFrame = wait.until(ExpectedConditions.frameToBeAvailableAndSwitchToIt("contents"))
    StockLogger.writeMessage("frame " + buttonNum + " displayed")
  }

  /**
    * 取引画面の板情報から現在値を取得する(プライスボードからではない!)
    * @return
    */
  def acquirePrice(): Price = {
    val wait = new WebDriverWait(driver, Duration.ofSeconds(60))
    var price = 0.0
    var askPrice = 0.0
    var amt = 0
    var done = false
    while (!done) {
      try {
        val reloadButton = wait.until(ExpectedConditions.visibilityOfElementLocated(By.id("psform:updcmd")))
        reloadButton.click()
        val priceCell = wait.until(ExpectedConditions.visibilityOfElementLocated(By.xpath("//*[@id='psform']/table/tbody/tr[3]/td[3]/table/tbody/tr[1]/td[2]/b")))
        val amtCell = wait.until(ExpectedConditions.visibilityOfElementLocated(By.xpath("//*[@id='psform']/table/tbody/tr[3]/td[3]/table/tbody/tr[6]/td[2]")))
        val askPriceCell = wait.until(ExpectedConditions.visibilityOfElementLocated(By.xpath("//*[@id='psform']/table/tbody/tr[3]/td[1]/table/tbody/tr[2]/td[2]/table/tbody/tr[11]/td[2]")))
        price = priceCell.getText.toDouble
        askPrice = askPriceCell.getText.toDouble
        amt = if (amtCell.getText == "--") 0 else amtCell.getText.toInt
        done = true
      } catch {
        case e: Exception =>
          showContentsFrame(0)
      }
    }
    // StockLogger.writeMessage(price +" "+ amt)

    new Price(LocalDateTime.now, price, askPrice, amt)
  }

  def close(): Unit = {
    try {
      driver.close()
    } catch { // まさかclose()が例外を投げることがあるとは…
      case e: Exception =>
        StockLogger.writeMessage(e.getMessage)
    }
  }

  def orderNewSell(amt: Int): Unit = {
    order(0, amt, newOrder = true)
    StockLogger.writeMessage("新規売 " + amt)
  }

  def orderNewBuy(amt: Int): Unit = {
    order(1, amt, newOrder = true)
    StockLogger.writeMessage("新規買 " + amt)
  }

  def orderSettleSell(amt: Int): Unit = {
    order(3, amt, newOrder = false)
    StockLogger.writeMessage("決済売 " + amt)
  }

  def orderSettleBuy(amt: Int): Unit = {
    order(4, amt, newOrder = false)
    StockLogger.writeMessage("決済買 " + amt)
  }

  def order(buttonNum: Int, amt: Int, newOrder: Boolean): Unit = {
    var done = false
    while (!done) {
      try {
        showContentsFrame(buttonNum)
        val wait = new WebDriverWait(driver, Duration.ofSeconds(60))
        // 決済時は建玉指定方法で自動指定を選択
        if (!newOrder) {
          val possort = wait.until(ExpectedConditions.elementToBeClickable(By.xpath("//*[@id='formOD0201:positionSortCls2:1']")))
          possort.click()
        }
        // 執行条件 決済時は//*[@id="formOD0201:conditionId"]
        val condPath = if (newOrder) "//*[@id='formOD0101:conditionId']" else "//*[@id='formOD0201:conditionId']"
        val cond = wait.until(ExpectedConditions.elementToBeClickable(By.xpath(condPath)))
        val sel = new Select(cond)
        sel.selectByValue("NARIYUKI_FAK")

        // 数量 決済時は//*[@id="formOD0201:qtyRcvd"]
        val qtyPath = if (newOrder) "//*[@id='formOD0101:qtyRcvd']" else "//*[@id='formOD0201:qtyRcvd']"
        val qty = wait.until(ExpectedConditions.elementToBeClickable(By.xpath(qtyPath)))
        qty.sendKeys(amt.toString)

        // 確認画面の省略 決済時は//*[@id="formOD0201:confirmId"]
        val confirmPath = if (newOrder) "//*[@id='formOD0101:confirmId']" else "//*[@id='formOD0201:confirmId']"
        val confirm = driver.findElement(By.xpath(confirmPath))
        confirm.click()

        // 注文 決済時は//*[@id="formOD0201:orderButtonId"]
        val orderPath = if (newOrder) "//*[@id='formOD0101:orderButtonId']" else "//*[@id='formOD0201:orderButtonId']"
        val order = wait.until(ExpectedConditions.elementToBeClickable(By.xpath(orderPath)))
        order.click()
        done = true
      } catch {
        case e: Exception => StockLogger.writeMessage(e.getMessage)
      }
    }
    showContentsFrame(0)
  }

}
