package observatory

object Main extends App {
  import org.apache.log4j.{Level, Logger}
  Logger.getLogger("org.apache.spark").setLevel(Level.WARN)

  val test = new Tile(0, 0, 0)
  val test1 = new Tile(255, 255, 8)
  println(Interaction.tileLocation(test))
  println(Interaction.tileLocation(test1))
  println(-710%360)
}
