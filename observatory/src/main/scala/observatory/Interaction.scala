package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import scala.math._
/**
  * 3rd milestone: interactive visualization
  */
object Interaction {

  /**
    * @param tile Tile coordinates
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(tile: Tile): Location = {
    val x = tile.x
    val y = tile.y
    val z = tile.zoom
    val lat = toDegrees(atan(sinh(Pi * (1.0 - 2.0 * y.toDouble / (1<<z)))))
    val lon = x.toDouble / (1<<z) * 360.0 - 180.0
//    if (lon >= 360 || lon <= - 360) {
//      lon = lon % 360
//    }
//    if (lon >= 180) lon = lon - 360
//    else if (lon < - 180) lon = lon + 360
//    if (lat >= 180 || lat <= -180) {
//      lat = lat % 180
//    }
//    if (lat > 90) lat = lat - 180
//    else if (lat <= -90) lat = lat + 180
    new Location(
      lat,
      lon)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @param tile Tile coordinates
    * @return A 256×256 image showing the contents of the given tile
    */
  def tile(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)], tile: Tile): Image = {
    val subTiles = for {
      idy <- 0 until 256
      idx <- 0 until 256
    } yield Tile(tile.x*256 + idx, tile.y*256 + idy, tile.zoom + 8)

    val locations = subTiles.par.map(subTile => tileLocation(subTile))

    val pixels = locations.map(loc => {
      val color = Visualization.interpolateColor(colors, Visualization.predictTemperature(temperatures, loc))
      Pixel(color.red, color.green, color.blue, 127)
    }).toArray

    Image(256, 256, pixels)
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    * @param yearlyData Sequence of (year, data), where `data` is some data associated with
    *                   `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
    yearlyData: Iterable[(Year, Data)],
    generateImage: (Year, Tile, Data) => Unit
  ): Unit = {
    yearlyData.par.foreach{case (year, data) => {
      val tiles = for {
        zoom <- 0 to 3
        x <- 0 until 1 << zoom
        y <-0 until 1<< zoom
      } yield Tile(x, y, zoom)
      tiles.foreach(tile => generateImage(year, tile, data))
    }}
  }
}
