package trader

import java.nio.file.{Files, Paths}

object Settings {
  val lines = Files.readAllLines(Paths.get("settings"))
  val id = lines.get(0)
  val pwd = lines.get(1)
  val pwd2 = lines.get(2)
}
