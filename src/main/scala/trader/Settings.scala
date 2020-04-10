package trader

import java.nio.file.{Files, Paths}

object Settings {
  private[trader] val lines = Files.readAllLines(Paths.get("settings"))
  private[trader] val id = lines.get(0)
  private[trader] val pwd = lines.get(1)
  private[trader] val pwd2 = lines.get(2)

  private[trader] val hakenThreshold = 450L
  private[trader] val hakenEnterThreshold = 35.0
  private[trader] val hakenWaitThreshold = 262L
  private[trader] val hakenDeclineThreshold = 35.0
  private[trader] val hakenScalpThreshold = 20.0
  private[trader] var replaymode = false
  private[trader] var showPrices = false

}
