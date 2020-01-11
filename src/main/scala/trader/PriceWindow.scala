package trader

import java.awt.{Color, Dimension, Graphics}
import java.time.LocalDateTime

import javax.swing._

object PriceWindow extends JFrame {
  private val w = 256
  private val h = 300
  private val hcenter = h / 2
  private val scaleFactor = 2.0 // pixel / yen
  // 基本的に価格の縦位置は固定だが、価格が変化した時だけ位置を変え、徐々に元の位置に戻すようにする
  private var yPriceOffset = 0 // 価格縦位置のオフセット
  private var yoffset = 0
  private var yprice = hcenter
  private var ym320 = hcenter
  private var ym640 = hcenter
  private var ym1280 = hcenter
  private var ym2560 = hcenter
  private var ydiffm320 = 0
  private var ydiffm640 = 0
  private var ydiffm1280 = 0
  private var ydiffm2560 = 0
  private var strprice = ""
  private var strm320 = ""
  private var strm640 = ""
  private var strm1280 = ""
  private var strm2560 = ""
  private var stramtrate = "0"
  private var strdiffamt = "0"
  private var strtime = ""

  def init(): Unit = {
    val p = PricePanel
    p.setSize(w, h)
    getContentPane.add(p)
    pack()
    setVisible(true)
  }

  def setData(time: LocalDateTime,
               price: Double, prevPrice: Double,
              m320: Double, m640: Double, m1280: Double, m2560: Double,
              diffm320: Double, diffm640: Double, diffm1280: Double, diffm2560: Double,
              amtrate: Double, diffamt: Int): Unit = {

    def toYpos(d: Double): Int = hcenter - ((d - price) * scaleFactor).round.toInt
    yprice = toYpos(price)
    ym320 = toYpos(m320)
    ym640 = toYpos(m640)
    ym1280 = toYpos(m1280)
    ym2560 = toYpos(m2560)

    def diff2Ylen(d: Double): Int = (d * 5 * scaleFactor).round.toInt

    ydiffm320 = diff2Ylen(diffm320)
    ydiffm640 = diff2Ylen(diffm640)
    ydiffm1280 = diff2Ylen(diffm1280)
    ydiffm2560 = diff2Ylen(diffm2560)

    def d2s(d: Double): String = d.round.toString

    strprice = d2s(price)
    strm320 = d2s(m320)
    strm640 = d2s(m640)
    strm1280 = d2s(m1280)
    strm2560 = d2s(m2560)
    stramtrate = d2s(amtrate)
    strdiffamt = diffamt.toString
    strtime = time.toString
    yPriceOffset -= ((price - prevPrice) * scaleFactor).toInt
    yPriceOffset = Math.max(yPriceOffset, 10 - hcenter)
    yPriceOffset = Math.min(yPriceOffset, hcenter)

    def range(l: List[Int]): (Int, Int) = (l.min, l.max)

    val r = range(List(yPriceOffset + hcenter, ym320, ym640, ym1280, ym2560))
    yoffset = if (r._1 < 0) yPriceOffset - r._1 else if (r._2 > h)
      yPriceOffset - (r._2 - h) else
      yPriceOffset
  }

  object PricePanel extends JPanel {
    override def getPreferredSize: Dimension = new Dimension(w, h)
    override def paint(g: Graphics): Unit = {
      val xorg = 50
      val xwidth = 40
      def x(n: Int): Int = xwidth * n + xorg
      super.paint(g)
      g.setColor(java.awt.Color.black)
      g.drawLine(x(0), hcenter + yoffset, x(1), ym320 + yoffset)
      g.drawLine(x(1), ym320 + yoffset, x(2), ym640 + yoffset)
      g.drawLine(x(2), ym640 + yoffset, x(3), ym1280 + yoffset)
      g.drawLine(x(3), ym1280 + yoffset, x(4), ym2560 + yoffset)

      g.drawLine(x(1), ym320 + yoffset, x(1), ym320 + yoffset - ydiffm320)
      g.drawLine(x(2), ym640 + yoffset, x(2), ym640 + yoffset - ydiffm640)
      g.drawLine(x(3), ym1280 + yoffset, x(3), ym1280 + yoffset - ydiffm1280)
      g.drawLine(x(4), ym2560 + yoffset, x(4), ym2560 + yoffset - ydiffm2560)

      g.drawString(strprice, x(0), hcenter + yoffset)
      g.drawString(strm320, x(1), ym320 + yoffset)
      g.drawString(strm640, x(2), ym640 + yoffset)
      g.drawString(strm1280, x(3), ym1280 + yoffset)
      g.drawString(strm2560, x(4), ym2560 + yoffset)
      g.drawString(stramtrate, 5, hcenter + yoffset)
      g.drawString(strdiffamt, 5, hcenter + yoffset + 10)
      g.drawString(strtime, x(1), 10)
    }
  }
}