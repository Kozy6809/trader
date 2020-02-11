package trader

import java.nio.file.{Files, Paths}

object Settings {
  private[trader] val lines = Files.readAllLines(Paths.get("settings"))
  private[trader] val id = lines.get(0)
  private[trader] val pwd = lines.get(1)
  private[trader] val pwd2 = lines.get(2)

  private[trader] var replaymode = false
}
