package trader

import java.awt.{Color, Dimension, Graphics}
import java.time.LocalDateTime

import javax.swing._

object PriceWindow extends JFrame {
  private val w = 256
  private val h = 300
  private def hcenter: Int = PricePanel.getHeight / 2
  private var centerPrice = 0.0
  private val scaleFactor = 2.0 // pixel / yen
  // 基本的に価格の縦位置は固定だが、価格が変化した時だけ位置を変え、徐々に元の位置に戻すようにする
  private var yoffset = 0
  private var yprice = hcenter
  private var ym5 = hcenter
  private var ym10 = hcenter
  private var ym20 = hcenter
  private var ym40 = hcenter
  private var haken = 0.0
  private var yhaken = 0
  private var ydiffm5 = 0
  private var ydiffm10 = 0
  private var ydiffm20 = 0
  private var ydiffm40 = 0
  private var strprice = ""
  private var strm5 = ""
  private var strm10 = ""
  private var strm20 = ""
  private var strm40 = ""
  private var strdiffm5 = ""
  private var strdiffm10 = ""
  private var strdiffm20 = ""
  private var strdiffm40 = ""
  private var stramtrate = "0"
  private var strdiffamt = "0"
  private var strtime = ""
  private var yrangemin = 0
  private var yrangemax = 0

  def init(): Unit = {
    val p = PricePanel
    p.setSize(w, h)
    getContentPane.add(p)
    pack()
    setVisible(true)
  }

  def setData(time: LocalDateTime,
               price: Double, prevPrice: Double,
              m5: Double, m10: Double, m20: Double, m40: Double,
              diffm5: Double, diffm10: Double, diffm20: Double, diffm40: Double,
              amtrate: Double, diffamt: Int): Unit = {

    if (centerPrice == 0.0) centerPrice = price
    if (Haken.newHaken) haken = Haken.hakens.head.p.askPrice

    def toYpos(d: Double): Int = hcenter - ((d - centerPrice) * scaleFactor).round.toInt
    yprice = toYpos(price)
    ym5 = toYpos(m5)
    ym10 = toYpos(m10)
    ym20 = toYpos(m20)
    ym40 = toYpos(m40)
    yhaken = if (haken > 0.0) toYpos(haken) else hcenter

    def diff2Ylen(d: Double): Int = (d * 5 * scaleFactor).round.toInt

    ydiffm5 = diff2Ylen(diffm5)
    ydiffm10 = diff2Ylen(diffm10)
    ydiffm20 = diff2Ylen(diffm20)
    ydiffm40 = diff2Ylen(diffm40)

    def d2s(d: Double): String = d.round.toString

    strprice = d2s(price)
    strm5 = d2s(m5)
    strm10 = d2s(m10)
    strm20 = d2s(m20)
    strm40 = d2s(m40)
    strdiffm5 = d2s(diffm5 * 10)
    strdiffm10 = d2s(diffm10 * 10)
    strdiffm20 = d2s(diffm20 * 10)
    strdiffm40 = d2s(diffm40 * 10)
    stramtrate = d2s(amtrate)
    strdiffamt = diffamt.toString
    strtime = time.toString

    yoffset = if (yprice <= 10) -yprice + 10
    else if (yprice >= hcenter * 2 - 10) (hcenter * 2 - 10) - yprice
    else 0
    centerPrice = centerPrice + yoffset.toDouble / scaleFactor
  }

  object PricePanel extends JPanel {
    override def getPreferredSize: Dimension = new Dimension(w, h)
    override def paint(g: Graphics): Unit = {
      val xorg = 50
      val xwidth = 40
      def x(n: Int): Int = xwidth * n + xorg
      super.paint(g)
      g.setColor(java.awt.Color.black)
      g.drawLine(x(0), yprice + yoffset, x(1), ym5 + yoffset)
      g.drawLine(x(1), ym5 + yoffset, x(2), ym10 + yoffset)
      g.drawLine(x(2), ym10 + yoffset, x(3), ym20 + yoffset)
      g.drawLine(x(3), ym20 + yoffset, x(4), ym40 + yoffset)

      g.drawLine(x(1), ym5 + yoffset, x(1), ym5 + yoffset - ydiffm5)
      g.drawLine(x(2), ym10 + yoffset, x(2), ym10 + yoffset - ydiffm10)
      g.drawLine(x(3), ym20 + yoffset, x(3), ym20 + yoffset - ydiffm20)
      g.drawLine(x(4), ym40 + yoffset, x(4), ym40 + yoffset - ydiffm40)

      g.drawString(strprice, x(0), yprice + yoffset)
      g.drawString(strm5, x(1), ym5 + yoffset)
      g.drawString(strdiffm5, x(1), ym5 + yoffset + 10)
      g.drawString(strm10, x(2), ym10 + yoffset)
      g.drawString(strdiffm10, x(2), ym10 + yoffset + 10)
      g.drawString(strm20, x(3), ym20 + yoffset)
      g.drawString(strdiffm20, x(3), ym20 + yoffset + 10)
      g.drawString(strm40, x(4), ym40 + yoffset)
      g.drawString(strdiffm40, x(4), ym40 + yoffset + 10)
      g.drawString(stramtrate, 5, yprice + yoffset)
      g.drawString(strdiffamt, 5, yprice + yoffset + 10)
      g.drawString(strtime, x(1), 10)

      g.setColor(java.awt.Color.red)
      g.setColor(java.awt.Color.blue)
      g.drawLine(0, yhaken + yoffset, w, yhaken + yoffset)
    }
  }
}