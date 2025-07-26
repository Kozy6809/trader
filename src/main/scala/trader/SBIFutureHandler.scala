package trader

import java.time.{Duration, LocalDateTime, LocalTime}

import org.openqa.selenium.support.ui.Select
import org.openqa.selenium.{By, Dimension, WebDriver}
//import org.openqa.selenium.chrome.{ChromeDriver, ChromeOptions}
import org.openqa.selenium.firefox._
import org.openqa.selenium.interactions.Actions
import org.openqa.selenium.support.ui.{ExpectedConditions, WebDriverWait}
import java.io.File
import _root_.com.titusfortner.logging._
import java.time.ZoneId
import org.openqa.selenium.WebElement
/**
  * SBI証券の先物サイトからデータをやりとりする
  *
  */
object SBIFutureHandler {
  // 2024/7/5サイトリニューアル以降は先物サイトのログイン画面にアクセスする
  val xpath_loginUrl = "https://site2.sbisec.co.jp/ETGate/?OutSide=on&_ControlID=WPLETsmR001Control&_DataStoreID=DSWPLETsmR001Control&sw_page=Future&cat1=home&cat2=none&getFlg=on"
  val xpath_logoutMenu = "//*[@id=\"header\"]/oms-header-board/div/oms-nav-header/div/div[1]/div[2]/a[1]"
  val xpath_logoutMsg = """/html/body/app-root/div/nz-spin/div/oms-logout/section/div/div[1]/span"""
  val xpath_meigara_header = """/html/body/app-root/div/nz-spin/div/oms-main/section/div[4]/as-split/as-split-area[1]/div/div/oms-price-board/div/section/div/div/div[1]/ul/li[1]"""
  // 重要なお知らせ画面
  val xpath_importantNoticeTitle = """/html/body/div[2]/table/tbody/tr/td[1]/div/div/div/h1"""
  val xpath_importantNoticeLink = """/html/body/div[2]/table/tbody/tr/td[1]/div/div/form[2]/ul[1]/li[2]/div[2]/div[2]/a"""
  val xpath_noticeAgreeButton = """/html/body/div[2]/table/tbody/tr/td[1]/form/div[4]/button"""
  // ロボット確認ポップアップ
  val xpath_confirmTitle = """/html/body/div[10]/div/div/div[2]/div/div/section/div/header/h1"""
  val xpath_confirmValue = """//*[@id="randomString"]"""
  val xpath_confirmInput = """//*[@id="userInput"]"""
  val xpath_confirmSubmit = """//*[@id="test"]"""
  // 認証コード画面
  val xpath_certifyTitle = """/html/body/div[5]/div[1]/div/div/h3"""
  val xpath_certifyCbox = """/html/body/div[5]/form/div[1]/div[1]/div[2]/input"""
  val xpath_certifyInput = """/html/body/div[5]/form/div[1]/div[2]/div/input"""
  val xpath_certifySubmit = """/html/body/div[5]/form/div[2]/input"""

  var driver: WebDriver = _
  // ロギングにはhttps://github.com/titusfortner/selenium-loggerを使用している
  // SeleniumLogger seleniumLogger = new SeleniumLogger()
  // seleniumLogger.setLevel(Level.WARNING)
  // new GeckoDriverLogger().setLevel(FirefoxDriverLogLevel.INFO)
  
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
    val options: FirefoxOptions = new FirefoxOptions()
    options.addArguments("-headless")
    val service: FirefoxDriverService =
    new GeckoDriverService.Builder().withLogFile(new File("./gecko.log"))
      .withLogLevel(FirefoxDriverLogLevel.DEBUG).build()
    service.start()
    driver = new FirefoxDriver(options);
    println(driver)
  }

  def attemptGenDriver(): Unit = {
    genDriver()
    close()
    println("no error in genDriver ")
  }

  def timer(sec: Long): WebDriverWait = new WebDriverWait(driver, Duration.ofSeconds(sec))

  /**
    * 指定されたXpathのエレメントが表示されたらEitherで返す。エラーはEither.Leftに格納される
    */
  def waitForElement(xpath: String, timeout: Long = 20L): Either[Exception, WebElement] = {
    val wait = timer(timeout)
    try {
      val element = wait.until(ExpectedConditions.visibilityOfElementLocated(By.xpath(xpath)))
      Right(element)
    } catch {
      case e: Exception =>
        StockLogger.writeMessage(s"SBIFutureHandler::waitForElement error: ${e.getMessage}")
        Left(e)
    }
  }

  def logout(): Unit = {
    StockLogger.writeMessage("attempt to logout")
    val logoutMenu = waitForElement(xpath_logoutMenu)
    logoutMenu.map(_.click()) 
    val logoutMsg = waitForElement(xpath_logoutMsg)
    if (logoutMsg.isLeft) {
      StockLogger.writeMessage("logout failed")
      close()
    } else {
      StockLogger.writeMessage("logout done.")
    }
  }

  def login(): Unit = {
    object  Status extends Enumeration {
      val LOGIN_INITIAL, LOGIN_CONT, MAIN, PRICEBOARD, CONTENTSFRAME, DONE, CERTIFICATION = Value
    }
    import Status._
    var status = LOGIN_INITIAL

    genDriver()
    println("UA文字列をチェックします")
    driver.get("https://testpage.jp/tool/ip_user_agent.php")
    val currentHTML = driver.getPageSource()
    StockLogger.writeMessage(currentHTML)
    println("サイトのページを保存しました")
    close()

    def doLogin(): Unit = {
      genDriver()
      StockLogger.writeMessage("attempt to initial login")
      // ログイン画面にアクセス
      // driver.get("https://www.sbisec.co.jp/ETGate/?OutSide=on&_ControlID=WPLETsmR001Control&_DataStoreID=DSWPLETsmR001Control&sw_page=Future&cat1=home&cat2=none&getFlg=on")

      // 2024/7/5サイトリニューアル以降は先物サイトのログイン画面に直接アクセスする。input要素は変化なし
      var loginDone = false
      while (!loginDone) {
        try {
          driver.get(xpath_loginUrl)
          loginDone = true
        } catch {
          case e: Exception =>
              StockLogger.writeMessage(e.getMessage())
              StockLogger.writeMessage("can't get login page")
              driver.close()
              genDriver()
              StockLogger.writeMessage("new driver acquired")
        }
      }
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
      driver.get(xpath_loginUrl)
      status = MAIN
    }

    def doMain(): Unit = {
      println(driver.getTitle)
      StockLogger.writeMessage("waiting main view")
      val meigara = waitForElement(xpath_meigara_header)
      if (meigara.isLeft) {
        StockLogger.writeMessage("メイン画面が表示されません。現在のHTMLソースを記録します")
        val currentHTML = driver.getPageSource()
        StockLogger.writeMessage(currentHTML)
        // ログインボタンを探す
        val loginList = driver.findElements(By.name("ACT_login"))
        if (loginList.size() > 0) {
          StockLogger.writeMessage("まだログイン画面です。ログインをやり直します。")
          status = LOGIN_INITIAL
        } else {
          StockLogger.writeMessage("メイン画面が表示されません。重要なお知らせをチェックします")
          val checkResult = clearAcknowledge()
          if (checkResult.isLeft) {
            StockLogger.writeMessage("想定外のエラーです")
            // 建玉が残っていればここで決済したいところだが、その術がない。携帯にアラートを送付するか
            close()
            TechAnal.save()
            System.exit(1)
          }
          if (checkResult.right.get) {
            StockLogger.writeMessage("お知らせを確認しました。ログイン画面にアクセスし、先物サイトが表示されるか確認します。")
            status = PRICEBOARD
          } else {
            status = CERTIFICATION
          }
        }
      } else {
        println(driver.getTitle)
        StockLogger.writeMessage("login done")
        status = PRICEBOARD
      }
    }

    def doCertify(): Unit = {
      println("ロボット確認画面をチェックします")
      val confirmValue = waitForElement(xpath_confirmValue)
      if (confirmValue.isRight) {
        println("ロボット確認コードが見つかりました")
        val confirmInput = waitForElement(xpath_confirmInput)
        confirmInput.right.get.sendKeys(confirmValue.right.get.getText)
        Thread.sleep(3000)
        val confirmSubmit = waitForElement(xpath_confirmSubmit)
        confirmSubmit.right.get.click()
        StockLogger.writeMessage("ロボット確認を通過しました")
        println("ロボット確認を通過しました")
      } else {
        StockLogger.writeMessage("ロボット確認画面は検出されませんでした")
        println("ロボット確認画面は検出されませんでした")
      }
      val certifyTitle = waitForElement(xpath_certifyTitle)
      if (certifyTitle.isLeft) {
        StockLogger.writeMessage("ログイン認証画面ではありません。処理を終了します。")
        println("ログイン認証画面ではありません。処理を終了します。")
        close()
        TechAnal.save()
        System.exit(1)
      }
      StockLogger.writeMessage("ログイン認証画面です")
        val scanner = new java.util.Scanner(System.in)
        println("認証メールを確認し、認証コードを入力してください")
        System.out.flush()
        val certifyString = scanner.nextLine()
        val certifyCbox = waitForElement(xpath_certifyCbox)
        certifyCbox.right.get.click() // チェックボックスをクリック
        Thread.sleep(3000)
        val certifyInput = waitForElement(xpath_certifyInput)
        certifyInput.right.get.sendKeys(certifyString) // 認証コードを入力
        Thread.sleep(3000)
        val certifySubmit = waitForElement(xpath_certifySubmit)
        certifySubmit.right.get.click() // 送信ボタンをクリック
        StockLogger.writeMessage("ログイン認証を通過しました")
        println("ログイン認証を通過しました")
        status = PRICEBOARD
  }

    def doPriceBoard(): Unit = {
      val element = priceBoard()
      if (element.isLeft) {
        StockLogger.writeMessage("can't display price board")
        close()
        status = LOGIN_INITIAL
      } else {
        StockLogger.writeMessage("price board displayed")
        status = DONE
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
        case CERTIFICATION => doCertify()
      }
    }
  }


  def priceBoard(): Either[Exception, WebElement] = {
    val firstRow = waitForElement(xpath_meigara_header)
    firstRow
  }

  /**
    * 重要なお知らせ画面が表示されていたら、すべての項目に同意し、trueを返す
    * 重要なお知らせ画面の取り扱いに失敗したらLeftで例外を返す
    * 重要なお知らせ画面でなければfalseを返す。この場合はログイン時の認証画面の可能性がある
    */
  def clearAcknowledge(): Either[Exception, Boolean] = {
    val noticeTitle = waitForElement(xpath_importantNoticeTitle)
    if (noticeTitle.isLeft) {
      StockLogger.writeMessage("重要なお知らせ画面以外です。ログイン認証画面かもしれません")
      Right(false)
    } else {
      val msgLink = waitForElement(xpath_importantNoticeLink)
      if (msgLink.isLeft) {
        StockLogger.writeMessage("重要なお知らせを処理できませんでした")
        Left(msgLink.left.get)
      } else {
        StockLogger.writeMessage("重要なお知らせを表示します: " + msgLink.right.get.getText)
        msgLink.right.get.click()
        // 確認ボタン
        val agreeButton = waitForElement(xpath_noticeAgreeButton)
        if (agreeButton.isLeft) {
          StockLogger.writeMessage("重要なお知らせを処理できませんでした")
          Left(msgLink.left.get)
        } else {
          StockLogger.writeMessage("重要なお知らせに同意します")
          agreeButton.right.get.click()
          // 確認ボタン押下後は確認ずみ画面→一覧表示画面に遷移する流れだが、一覧画面からメイン画面に行けないため、
          // ここで再ログインシーケンスに移行する
          driver.get(xpath_loginUrl) // ログイン画面にアクセスする。すでにログイン済みなので、これによってメイン画面が表示される
          Right(true)
        }
      }
    }
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
        val priceCell = wait.until(ExpectedConditions.visibilityOfElementLocated(
        By.xpath("//*[@id=\"tableHeight\"]/li/div[2]/div/div[1]/div")))
        val amtCell = wait.until(ExpectedConditions.visibilityOfElementLocated(
        By.xpath("//*[@id=\"tableHeight\"]/li/div[3]/div/div[2]/div")))
        val askPriceCell = wait.until(ExpectedConditions.visibilityOfElementLocated(
        By.xpath("//*[@id=\"tableHeight\"]/li/div[9]/ul/li[1]/span[3]")))

        price = priceCell.getText.replaceAll(",", "").toDouble
        askPrice = askPriceCell.getText.replaceAll(",", "").toDouble
        amt = if (amtCell.getText == "--") 0 else amtCell.getText.replaceAll(",", "").toInt
        done = true
      } catch {
        case e: Exception => {
          errnum += 1
          if (errnum > 3) {
            StockLogger.writeMessage(s"SBIFutureHandler::error exceeds limit times")
            driver.quit()
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

    new Price(LocalDateTime.now(ZoneId.of("Asia/Tokyo")), price, askPrice, amt)
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
