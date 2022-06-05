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
    System.setProperty("webdriver.gecko.driver", Settings.driverLocation)
    val firefoxOptions = new FirefoxOptions
    firefoxOptions.setBinary(firefoxBinary)
    var done = false
    while (!done) {
      try {
        driver = new FirefoxDriver(firefoxOptions)
        done = true
      } catch {
        case e: Exception =>
          StockLogger.writeMessage(s"SBIFutureHander::genDriver ${e.getMessage}")
          Thread.sleep(6000)
      }
    }
    println(driver)
  }

  def attemptGenDriver(): Unit = {
    genDriver()
    close()
    println("no error in genDriver ")
  }

  def login(): Unit = {
    object  Status extends Enumeration {
      val LOGIN_INITIAL, LOGIN_CONT, MAIN, PRICEBOARD, CONTENTSFRAME, DONE = Value
    }
    import Status._
    var status = LOGIN_INITIAL

    def doLogin(): Unit = {
      genDriver()
      val wait = new WebDriverWait(driver, Duration.ofSeconds(60))
      StockLogger.writeMessage("attempt to initial login")
      // ログイン画面にアクセス
      driver.get("https://www.sbisec.co.jp/ETGate/?OutSide=on&_ControlID=WPLETsmR001Control&_DataStoreID=DSWPLETsmR001Control&sw_page=Future&cat1=home&cat2=none&getFlg=on")
      // ID入力
      driver.findElement(By.name("user_id")).sendKeys(Settings.id)
      // パスワード入力
      driver.findElement(By.name("user_password")).sendKeys(Settings.pwd)
      // ログインボタンクリック
      val login = driver.findElement(By.name("ACT_login"))
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
      StockLogger.writeMessage("login done. wait for main view")
      status = MAIN
    }

    def doLoginContinuous(): Unit = {
      genDriver()
      val wait = new WebDriverWait(driver, Duration.ofSeconds(60))
      StockLogger.writeMessage("attempt to login")
      // ログイン画面にアクセス
      driver.get("https://www.sbisec.co.jp/ETGate/?OutSide=on&_ControlID=WPLETsmR001Control&_DataStoreID=DSWPLETsmR001Control&sw_page=Future&cat1=home&cat2=none&getFlg=on")
      status = MAIN
    }

    def doMain(): Unit = {
      println(driver.getTitle)
      val wait = new WebDriverWait(driver, Duration.ofSeconds(60))
      try {
        StockLogger.writeMessage("waiting main view")
        wait.until(ExpectedConditions.visibilityOfElementLocated(By.name("main")))
        status = PRICEBOARD
      } catch {
        case e: Exception =>
          StockLogger.writeMessage(e.getMessage)
          StockLogger.writeMessage("メイン画面が表示されません。重要なお知らせをチェックします")
          try {
            clearAcknowledge()
            wait.until(ExpectedConditions.visibilityOfElementLocated(By.name("main")))
            status = PRICEBOARD
          } catch {
            case e: Exception =>
              StockLogger.writeMessage("想定外のエラーです")
              // 建玉が残っていればここで決済したいところだが、その術がない。携帯にアラートを送付するか
              close()
              TechAnal.save()
              System.exit(1)
          }
      }
      println(driver.getTitle)
      StockLogger.writeMessage("login done")
      loginTime = LocalDateTime.now()
      status = PRICEBOARD
    }

    def doPriceBoard(): Unit = {
      try {
        priceBoard()
        StockLogger.writeMessage("price board displayed")
        status = CONTENTSFRAME
      } catch {
        case e: Exception =>
          StockLogger.writeMessage("can't display price board")
          close()
//          status = LOGIN_CONT
          status = LOGIN_INITIAL
      }
    }

    def doContentsFrame(): Unit = {
      try {
        showContentsFrame(0)
        StockLogger.writeMessage("new SELL screen displayed")
        status = DONE
      } catch {
        case e: Exception =>
          StockLogger.writeMessage(e.getClass.toString)
          StockLogger.writeMessage(s"SBIFutureHandler::doContentsFrame ${e.getMessage.split("\n")(0)}")
          close()
//          status = LOGIN_CONT
          status = LOGIN_INITIAL
      }
    }

    while (status != DONE) {
      status match {
        case LOGIN_INITIAL => doLogin()
        case LOGIN_CONT => doLoginContinuous()
        case MAIN => doMain()
        case PRICEBOARD => doPriceBoard()
        case CONTENTSFRAME => doContentsFrame()
      }
    }
  }

  def priceBoard(): Unit = {
    val wait = new WebDriverWait(driver, Duration.ofSeconds(60))

    driver.switchTo().defaultContent()
    val mainFrame = wait.until(ExpectedConditions.frameToBeAvailableAndSwitchToIt("main"))
    val priceFrame = wait.until(ExpectedConditions.frameToBeAvailableAndSwitchToIt("price"))
    val firstRow = wait.until(ExpectedConditions.visibilityOfElementLocated(By.xpath("//*[@id='datagrid-row-r7-2-0']")))

  }

  /**
    * 重要なお知らせ画面が表示されていたら、すべての項目に同意する
    * 重要なお知らせ画面でなければ例外を投げる
    */
  def clearAcknowledge(): Unit = {
    val wait = new WebDriverWait(driver, Duration.ofSeconds(60))
    try {
      val noticemsg = wait.until(ExpectedConditions.visibilityOfElementLocated(
        By.xpath("/html/body/div[1]/table/tbody/tr/td[1]/table/tbody/tr[2]/td[2]/table/tbody/tr[3]/td/div/b")))
      StockLogger.writeMessage("重要なお知らせ: " + noticemsg.getText)
    } catch {
      case e: Exception =>
        StockLogger.writeMessage("重要なお知らせ画面以外のエラーです")
        throw e
    }
    // 同時に複数のメッセージがポストされたら以下の処理をリピートしなければならないが、これまでそのようなケースは起きていない
    try {
      // 最新メッセージの表示
      val msglnk = wait.until(ExpectedConditions.visibilityOfElementLocated(
        By.xpath("/html/body/div[1]/table/tbody/tr/td[1]/table/tbody/tr[2]/td[2]/form/table[4]/tbody/tr/td/table/tbody/tr[2]/td[2]/table/tbody/tr[2]/td/a")))
      StockLogger.writeMessage("重要なお知らせを表示します: " + msglnk.getText)
      msglnk.click()
      val agreebtn = wait.until(ExpectedConditions.visibilityOfElementLocated(
        By.xpath("/html/body/div[1]/table/tbody/tr/td[1]/table/tbody/tr/td[2]/form/table[4]/tbody/tr/td/input[1]")))
      StockLogger.writeMessage("重要なお知らせに同意します")
      agreebtn.click()
      val backbtn = wait.until(ExpectedConditions.visibilityOfElementLocated(
        By.xpath("/html/body/div/table/tbody/tr/td[1]/table/tbody/tr/td[2]/form/table[7]/tbody/tr[4]/td/input[1]")))
      StockLogger.writeMessage("重要なお知らせ一覧に戻ります")
      backbtn.click()
    } catch {
      case e: Exception =>
        StockLogger.writeMessage("重要なお知らせを処理できませんでした")
        throw e
    }
    val nomsg = wait.until(ExpectedConditions.visibilityOfElementLocated(
      By.xpath("/html/body/div[1]/table/tbody/tr/td[1]/table/tbody/tr[2]/td[2]/form/table[3]/tbody/tr/td/table/tbody/tr[2]/td/div/b")))
    StockLogger.writeMessage("重要なお知らせはもうありません: " + nomsg.getText)
    // ログイン画面にアクセスする。すでにログイン済みなので、これによってメイン画面が表示される
    driver.get("https://www.sbisec.co.jp/ETGate/?OutSide=on&_ControlID=WPLETsmR001Control&_DataStoreID=DSWPLETsmR001Control&sw_page=Future&cat1=home&cat2=none&getFlg=on")
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
   * 2022/5からのAngularが画面に対応するバージョン
   * 旧版では取引画面の板情報から現在値を取得(プライスボードからではない!)
   * していたが、今回はプライスボードからの取得を試みる
   * 以前はプライスボードからの取得に不具合があったものと思うが思い出せない
   * 画面更新のタイミングが制御できなかったとかかも
    *
    * @return p Price
    */
  def acquirePrice(): Price = {
    val wait = new WebDriverWait(driver, Duration.ofSeconds(60))
    var price = 0.0
    var askPrice = 0.0
    var amt = 0
    var done = false
    var errnum = 0
    while (!done) {
      try {
        // 旧版
        // val reloadButton = wait.until(ExpectedConditions.visibilityOfElementLocated(By.id("psform:updcmd")))
        // reloadButton.click()
        // val priceCell = wait.until(ExpectedConditions.visibilityOfElementLocated(By.xpath("//*[@id='psform']/table/tbody/tr[3]/td[3]/table/tbody/tr[1]/td[2]/b")))
        // val amtCell = wait.until(ExpectedConditions.visibilityOfElementLocated(By.xpath("//*[@id='psform']/table/tbody/tr[3]/td[3]/table/tbody/tr[6]/td[2]")))
        // val askPriceCell = wait.until(ExpectedConditions.visibilityOfElementLocated(By.xpath("//*[@id='psform']/table/tbody/tr[3]/td[1]/table/tbody/tr[2]/td[2]/table/tbody/tr[11]/td[2]")))

        val priceCell = wait.until(ExpectedConditions.visibilityOfElementLocated(By.xpath("/html/body/app-root/div/nz-spin/div/oms-main/section/div[3]/as-split/as-split-area[1]/div/div/oms-price-board/div/section/div/div/div[2]/ul/li/div[2]/div/div[1]/div")))
        println("price cell found")
        val amtCell = wait.until(ExpectedConditions.visibilityOfElementLocated(By.xpath("/html/body/app-root/div/nz-spin/div/oms-main/section/div[3]/as-split/as-split-area[1]/div/div/oms-price-board/div/section/div/div/div[2]/ul/li/div[3]/div/div[2]/div")))
        println("amount cell found")
        val askPriceCell = wait.until(ExpectedConditions.visibilityOfElementLocated(By.xpath("/html/body/app-root/div/nz-spin/div/oms-main/section/div[3]/as-split/as-split-area[1]/div/div/oms-price-board/div/section/div/div/div[2]/ul/li/div[9]/ul/li[1]/span[3]")))
        println("ask price cell found")
        price = priceCell.getText.toDouble
        askPrice = askPriceCell.getText.toDouble
        amt = if (amtCell.getText == "--") 0 else amtCell.getText.toInt
        done = true
      } catch {
        case e: Exception => {
          errnum += 1
          StockLogger.writeMessage(s"SBIFutureHandler::acquirePrice error ${errnum} times")
          StockLogger.writeMessage(e.getClass.toString)
          StockLogger.writeMessage(s"SBIFutureHandler::acquirePrice ${e.getMessage.split("\n")(0)}")
          try {
            showContentsFrame(0)
          } catch {
            case e: Exception => {
              StockLogger.writeMessage(e.getClass.toString)
              StockLogger.writeMessage(s"SBIFutureHandler::showContentsFrame ${e.getMessage.split("\n")(0)}")
              throw e
            }
          }
        }
      }
    }
    // StockLogger.writeMessage(price +" "+ amt)

    new Price(LocalDateTime.now, price, askPrice, amt)
  }

  def close(): Unit = {
    try {
      driver.quit()
    } catch { // まさかclose()が例外を投げることがあるとは…
      case e: Exception =>
        StockLogger.writeMessage(s"SBIFutureHandler::close ${e.getMessage}")
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
