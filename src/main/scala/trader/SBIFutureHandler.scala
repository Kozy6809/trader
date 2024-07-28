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
  // 2024/7/5サイトリニューアル以降は先物サイトのログイン画面にアクセスする
  val loginUrl = "https://site2.sbisec.co.jp/ETGate/?OutSide=on&_ControlID=WPLETsmR001Control&_DataStoreID=DSWPLETsmR001Control&sw_page=Future&cat1=home&cat2=none&getFlg=on"
  var driver: WebDriver = _
  
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

  def timer(sec: Long): WebDriverWait = new WebDriverWait(driver, Duration.ofSeconds(sec))

  def logout(): Unit = {
    val wait = timer(60)
    wait.pollingEvery(1, TimeUnit.SECONDS)
    try {
      StockLogger.writeMessage("attempt to logout")
      driver.findElement(By.xpath("""//*[@id="header"]/oms-header-board/div/div/oms-nav-header/div/div[1]/a)"""")).click()
      wait.until(ExpectedConditions.visibilityOfElementLocated(By.xpath("""/html/body/app-root/div/nz-spin/div/oms-logout/section/div/div[1]/span""")))
      StockLogger.writeMessage("logout done.")
    } catch {
      case e: Exception =>
        StockLogger.writeMessage(e.getMessage())
        StockLogger.writeMessage("logout failed")
        close()
    }
  }

  def login(): Unit = {
    object  Status extends Enumeration {
      val LOGIN_INITIAL, LOGIN_CONT, MAIN, PRICEBOARD, CONTENTSFRAME, DONE = Value
    }
    import Status._
    var status = LOGIN_INITIAL

    def doLogin(): Unit = {
      genDriver()
      StockLogger.writeMessage("attempt to initial login")
      // ログイン画面にアクセス
      // driver.get("https://www.sbisec.co.jp/ETGate/?OutSide=on&_ControlID=WPLETsmR001Control&_DataStoreID=DSWPLETsmR001Control&sw_page=Future&cat1=home&cat2=none&getFlg=on")

      // 2024/7/5サイトリニューアル以降は先物サイトのログイン画面に直接アクセスする。input要素は変化なし
      driver.get(loginUrl)
      // ID入力
      driver.findElement(By.name("user_id")).sendKeys(Settings.id)
      // パスワード入力
      driver.findElement(By.name("user_password")).sendKeys(Settings.pwd)
      // ログインボタンクリック
      val login = driver.findElement(By.name("ACT_login"))
      timer(60).until(ExpectedConditions.elementToBeClickable(login))
      // ポップアップでクリックボタンが押せない時、ポップアップが消えるのを待つ
      var done = false
      while (!done) {
        try {
          login.click()
          done = true
        } catch {
          case e: Exception =>
            StockLogger.writeMessage(e.getMessage())
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
      driver.get(loginUrl)
      status = MAIN
    }

    def doMain(): Unit = {
      println(driver.getTitle)
      val wait = timer(60)
      try {
        StockLogger.writeMessage("waiting main view")
        // wait.until(ExpectedConditions.visibilityOfElementLocated(By.name("main")))
        wait.pollingEvery(2, TimeUnit.SECONDS) // デフォルトは500msecなのでbangされる? それとも関係ない?
        wait.until(ExpectedConditions.visibilityOfElementLocated(By.xpath("""//*[@id="header"]/oms-header-board/div/div/oms-nav-header""")))
        status = PRICEBOARD
      } catch {
        case e: Exception =>
          StockLogger.writeMessage(e.getMessage)
          StockLogger.writeMessage("メイン画面が表示されません。重要なお知らせをチェックします")
          try {
            clearAcknowledge()
            // wait.until(ExpectedConditions.visibilityOfElementLocated(By.name("main")))
            wait.until(ExpectedConditions.visibilityOfElementLocated(By.xpath("""//*[@id="header"]/oms-header-board/div/div/oms-nav-header""")))
            status = PRICEBOARD
          } catch {
            case e: Exception =>
              StockLogger.writeMessage(e.getMessage())
              StockLogger.writeMessage("想定外のエラーです")
              // 建玉が残っていればここで決済したいところだが、その術がない。携帯にアラートを送付するか
              close()
              TechAnal.save()
              System.exit(1)
          }
      }
      println(driver.getTitle)
      StockLogger.writeMessage("login done")
      status = PRICEBOARD
    }

    def doPriceBoard(): Unit = {
      try {
        priceBoard()
        StockLogger.writeMessage("price board displayed")
        status = DONE
      } catch {
        case e: Exception =>
          StockLogger.writeMessage(e.getMessage())
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
    val wait = timer(60)

    // driver.switchTo().defaultContent()
    // // !!! TODO 2022/5のAngular移行でエレメントのpathが変わったはず。確認する!!
    // val mainFrame = wait.until(ExpectedConditions.frameToBeAvailableAndSwitchToIt("main"))
    // val priceFrame = wait.until(ExpectedConditions.frameToBeAvailableAndSwitchToIt("price"))
    // val firstRow = wait.until(ExpectedConditions.visibilityOfElementLocated(By.xpath("//*[@id='datagrid-row-r7-2-0']")))
    val firstRow = wait.until(ExpectedConditions.visibilityOfElementLocated(
    By.xpath("""/html/body/app-root/div/nz-spin/div/oms-main/section/div[3]/as-split/as-split-area[1]/div/div/oms-price-board""")))
}

  /**
    * 重要なお知らせ画面が表示されていたら、すべての項目に同意する
    * 重要なお知らせ画面でなければ例外を投げる
    */
  def clearAcknowledge(): Unit = {
    val wait = timer(60)
    try {
      // 2024/7/8
      // サイトリニューアルに伴い、重要なお知らせ画面の確認方法を変更する
      val noticeTitle = wait.until(ExpectedConditions.visibilityOfElementLocated(
        By.xpath("/html/body/div[2]/table/tbody/tr/td[1]/table/tbody/tr[2]/td[2]/table/tbody/tr[3]/td/div/b")))
      StockLogger.writeMessage(noticeTitle.getText()) // 正常なら「重要なお知らせ」となる

      // // "重要なお知らせ"のpath。2022/6/11 従前から変わっていないことを確認した。
      // val noticemsg = wait.until(ExpectedConditions.visibilityOfElementLocated(
      //   By.xpath("/html/body/div[1]/table/tbody/tr/td[1]/table/tbody/tr[2]/td[2]/table/tbody/tr[3]/td/div/b")))
      // StockLogger.writeMessage("重要なお知らせ: " + noticemsg.getText)
    } catch {
      case e: Exception =>
        StockLogger.writeMessage(e.getMessage())
        StockLogger.writeMessage("重要なお知らせ画面以外のエラーです")
        throw e
    }
    // 同時に複数のメッセージがポストされたら以下の処理をリピートしなければならないが、これまでそのようなケースは起きていない
    try {
      // 最新メッセージの表示
      // 最新メッセージへのリンクのpath。2022/6/11 従前から変わっていないことを確認した。
      // val msglnk = wait.until(ExpectedConditions.visibilityOfElementLocated(
      //   By.xpath("/html/body/div[1]/table/tbody/tr/td[1]/table/tbody/tr[2]/td[2]/form/table[4]/tbody/tr/td/table/tbody/tr[2]/td[2]/table/tbody/tr[2]/td/a")))
      // 2024/7/5 サイトリニューアル以降のパス
      val msglnk = wait.until(ExpectedConditions.visibilityOfElementLocated(
        By.xpath("/html/body/div[2]/table/tbody/tr/td[1]/table/tbody/tr[2]/td[2]/form/table[4]/tbody/tr/td/table/tbody/tr[2]/td[2]/table/tbody/tr[2]/td/a")))
      StockLogger.writeMessage("重要なお知らせを表示します: " + msglnk.getText)
      msglnk.click()
      // 同意ボタンのpath。2022/6/11 従前から変わっていないことを確認した。
      // val agreebtn = wait.until(ExpectedConditions.visibilityOfElementLocated(
      //   By.xpath("/html/body/div[1]/table/tbody/tr/td[1]/table/tbody/tr/td[2]/form/table[4]/tbody/tr/td/input[1]")))
      // StockLogger.writeMessage("重要なお知らせに同意します")
      // 2024/7/5 サイトリニューアル以降のパス
      val agreebtn = wait.until(ExpectedConditions.visibilityOfElementLocated(
        By.xpath("/html/body/div[2]/table/tbody/tr/td[1]/table/tbody/tr/td[2]/form/table[4]/tbody/tr/td/input[1]")))
      StockLogger.writeMessage("重要なお知らせに同意します")
      agreebtn.click()
      // 2024/7/5 以下の手順はスキップし、ログインシーケンスに戻るようにする
      
      // 戻るボタンのpath。2022/6/11 従前から変わっていないことを確認した。
      // val backbtn = wait.until(ExpectedConditions.visibilityOfElementLocated(
      //   By.xpath("/html/body/div[1]/table/tbody/tr/td[1]/table/tbody/tr/td[2]/form/table[7]/tbody/tr[4]/td/input[1]")))
      // StockLogger.writeMessage("重要なお知らせ一覧に戻ります")
      // backbtn.click()
    } catch {
      case e: Exception =>
        StockLogger.writeMessage(e.getMessage())
        StockLogger.writeMessage("重要なお知らせを処理できませんでした")
        throw e
    }
      // メッセージ無し表示のpath。2022/6/11 従前から変わっていないことを確認した。
    // val nomsg = wait.until(ExpectedConditions.visibilityOfElementLocated(
    //   By.xpath("/html/body/div[1]/table/tbody/tr/td[1]/table/tbody/tr[2]/td[2]/form/table[3]/tbody/tr/td/table/tbody/tr[2]/td/div/b")))
    // StockLogger.writeMessage("重要なお知らせはもうありません: " + nomsg.getText)

    // ログイン画面にアクセスする。すでにログイン済みなので、これによってメイン画面が表示される
    // driver.get("https://www.sbisec.co.jp/ETGate/?OutSide=on&_ControlID=WPLETsmR001Control&_DataStoreID=DSWPLETsmR001Control&sw_page=Future&cat1=home&cat2=none&getFlg=on")
    // 2024/7/5以降、先物サイトに直接ログインする
    driver.get(loginUrl)
  }

  /**
    * プライスボードの取引ボタンをクリックして取引画面を表示する。例外を投げる可能性あり
    * !!! TODO 2022/5のAngular移行でエレメントのpathが変わったはず。確認する!!
    * @param buttonNum 0: 新規売 1: 新規買 3: 決済売 4: 決済買
    * @return
    */
  def showContentsFrame(buttonNum: Int): Unit = {
    val wait = timer(60)
    // val firstRow = wait.until(ExpectedConditions.visibilityOfElementLocated(By.xpath("//*[@id='datagrid-row-r7-2-0']")))
    val firstRow = wait.until(ExpectedConditions.visibilityOfElementLocated(
    By.xpath("/html/body/app-root/div/nz-spin/div/oms-main/section/div[3]/as-split/as-split-area[1]/div/div/oms-price-board")))
    //*[@id="tableHeight"]/li/div[11]/ul/li[1]/button[1]
    
    // 新規売 "//*[@id="tableHeight"]/li/div[11]/ul/li[1]/button[1]"
    // 新規買 "//*[@id="tableHeight"]/li/div[11]/ul/li[1]/button[2]"
    // 決済売 "//*[@id="tableHeight"]/li/div[11]/ul/li[2]/button[1]"
    // 決済買 "//*[@id="tableHeight"]/li/div[11]/ul/li[2]/button[2]"
    val buttonPath = buttonNum match {
      case 0 => """//*[@id="tableHeight"]/li/div[11]/ul/li[1]/button[1]""" // 新規売
      case 1 => """//*[@id="tableHeight"]/li/div[11]/ul/li[1]/button[2]""" // 新規買
      case 3 => """//*[@id="tableHeight"]/li/div[11]/ul/li[2]/button[1]""" // 決済売
      case 4 => """//*[@id="tableHeight"]/li/div[11]/ul/li[2]/button[2]""" // 決済買
    }
    val tradeButton = firstRow.findElement(By.xpath(buttonPath))
    tradeButton.click()
    // driver.switchTo().parentFrame()
    // val contentsFrame = wait.until(ExpectedConditions.frameToBeAvailableAndSwitchToIt("contents"))

    // TODO 新規売りと新規買いで確認ボタンのパスは変わらない。決済では変わるはずなので処理を分ける必要あり
    // 新規の場合のパス: //*[@id="scrollBottom"]/div/oms-new-future-order/section/div[1]/div[4]/oms-order-input/section/div/div[3]/div[2]/button
    val btnConfirm = wait.until(ExpectedConditions.visibilityOfElementLocated(
        By.xpath("""//*[@id="scrollBottom"]/div/oms-new-future-order/section/div[1]/div[4]/oms-order-input/section/div/div[3]/div[2]/button""")))

    StockLogger.writeMessage("frame " + buttonNum + " displayed")
  }

  /**
   * 2022/5からのAngularにより生成される画面に対応するバージョン
   * 旧版では取引画面の板情報から現在値を取得(プライスボードからではない!)
   * していたが、今回はプライスボードからの取得を試みる
   * 以前はプライスボードからの取得に不具合があったものと思うが思い出せない
   * 画面更新のタイミングが制御できなかったとかかも
    *
    * @return p Price
    */
  def acquirePrice(): Price = {
    val wait = timer(60)
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

        // 2024/7/5のサイトリニューアル以前のパス
        // val priceCell = wait.until(ExpectedConditions.visibilityOfElementLocated(
        // By.xpath("/html/body/app-root/div/nz-spin/div/oms-main/section/div[3]/as-split/as-split-area[1]/div/div/oms-price-board/div/section/div/div/div[2]/ul/li/div[2]/div/div[1]/div")))
        // val amtCell = wait.until(ExpectedConditions.visibilityOfElementLocated(
        // By.xpath("/html/body/app-root/div/nz-spin/div/oms-main/section/div[3]/as-split/as-split-area[1]/div/div/oms-price-board/div/section/div/div/div[2]/ul/li/div[3]/div/div[2]/div")))
        // val askPriceCell = wait.until(ExpectedConditions.visibilityOfElementLocated(
        // By.xpath("/html/body/app-root/div/nz-spin/div/oms-main/section/div[3]/as-split/as-split-area[1]/div/div/oms-price-board/div/section/div/div/div[2]/ul/li/div[9]/ul/li[1]/span[3]")))
        // 2024/7/5以降もパスは変化なし
        val priceCell = wait.until(ExpectedConditions.visibilityOfElementLocated(
        By.xpath("/html/body/app-root/div/nz-spin/div/oms-main/section/div[3]/as-split/as-split-area[1]/div/div/oms-price-board/div/section/div/div/div[2]/ul/li/div[2]/div/div[1]/div")))
        val amtCell = wait.until(ExpectedConditions.visibilityOfElementLocated(
        By.xpath("/html/body/app-root/div/nz-spin/div/oms-main/section/div[3]/as-split/as-split-area[1]/div/div/oms-price-board/div/section/div/div/div[2]/ul/li/div[3]/div/div[2]/div")))
        val askPriceCell = wait.until(ExpectedConditions.visibilityOfElementLocated(
        By.xpath("/html/body/app-root/div/nz-spin/div/oms-main/section/div[3]/as-split/as-split-area[1]/div/div/oms-price-board/div/section/div/div/div[2]/ul/li/div[9]/ul/li[1]/span[3]")))
        price = priceCell.getText.replaceAll(",", "").toDouble
        askPrice = askPriceCell.getText.replaceAll(",", "").toDouble
        amt = if (amtCell.getText == "--") 0 else amtCell.getText.replaceAll(",", "").toInt
        done = true
      } catch {
        case e: Exception => {
          errnum += 1
          if (errnum > 3) {
            StockLogger.writeMessage(s"SBIFutureHandler::error exceeds limit times")
            driver.close()
            System.exit(1)
          }
          StockLogger.writeMessage(s"SBIFutureHandler::acquirePrice error ${errnum} times")
          StockLogger.writeMessage(e.getClass.toString)
          StockLogger.writeMessage(s"SBIFutureHandler::acquirePrice ${e.getMessage.split("\n")(0)}")
          driver.navigate().refresh()
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
  // TODO エレメントのパスが2022/5のAngular移行で変化した可能性あり。確認すること
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
