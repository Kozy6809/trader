package trader

class TrendStrategy extends Strategy {
  object  Status extends Enumeration {
    val MAY_ENTER, IN_THERE, MAY_CHANGE, OUT_THERE = Value
  }
  private var status = Status.OUT_THERE
  private var holding = Judgement.STAY
  private var positionPrice = 0.0

  override def add(p: Price): Judgement.Value = Judgement.STAY

  override def reset(): Unit =  {
    status = Status.OUT_THERE
    holding = Judgement.STAY
    positionPrice = 0.0
  }

  private def analyze(): Status.Value = {
    /*
    開始条件:
    ・ボックスレンジから2ティックはみ出した
     */
    Status.OUT_THERE
  }
}
