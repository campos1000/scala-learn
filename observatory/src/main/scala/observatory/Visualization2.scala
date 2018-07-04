package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import observatory.utils._

/**
  * 5th milestone: value-added information visualization
  */
object Visualization2 {

  /**
    * @param point (x, y) coordinates of a point in the grid cell
    * @param d00 Top-left value
    * @param d01 Bottom-left value
    * @param d10 Top-right value
    * @param d11 Bottom-right value
    * @return A guess of the value at (x, y) based on the four known values, using bilinear interpolation
    *         See https://en.wikipedia.org/wiki/Bilinear_interpolation#Unit_Square
    */
  def bilinearInterpolation(
    point: CellPoint,
    d00: Temperature,
    d01: Temperature,
    d10: Temperature,
    d11: Temperature
  ): Temperature =
    d00*(1-point.x)*(1-point.y) + d10*(point.x)*(1-point.y) + d01*(1-point.x)*point.y + d11*point.x*point.y

  /**
    * @param grid Grid to visualize
    * @param colors Color scale to use
    * @param tile Tile coordinates to visualize
    * @return The image of the tile at (x, y, zoom) showing the grid using the given color scale
    */
  def visualizeGrid(
    grid: GridLocation => Temperature,
    colors: Iterable[(Temperature, Color)],
    tile: Tile
  ): Image = {
    val subTiles = for {
      idy <- 0 until 256
      idx <- 0 until 256
    } yield Tile(tile.x*256 + idx, tile.y*256 + idy, tile.zoom + 8)

    val locations = subTiles.map(subTile => Interaction.tileLocation(subTile))

    val pixels = locations.map(loc => {
      val baseGrid = Utils.locationToGridIndex(loc)
      val cellPoint = new CellPoint(loc.lon - baseGrid.lon, loc.lat - baseGrid.lat)
      val color = Visualization.interpolateColor(colors,
        bilinearInterpolation(cellPoint,
          grid(baseGrid),
          grid(new GridLocation(baseGrid.lat + 1, baseGrid.lon)),
          grid(new GridLocation(baseGrid.lat, baseGrid.lon + 1)),
          grid(new GridLocation(baseGrid.lat + 1, baseGrid.lon+ 1))
        )
      )
      Pixel(color.red, color.green, color.blue, 127)
    }).toArray

    Image(256, 256, pixels)
  }

}
