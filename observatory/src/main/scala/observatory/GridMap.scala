package observatory

/**
  * Created by Tuan on 2018/06/28.
  */
class GridMap(temperatures: Iterable[(Location, Temperature)]) {
  var calculated = Array.ofDim[Boolean](180,360)
  var cached = Array.ofDim[Temperature](180, 360)

  def getTemperature(gridLocation: GridLocation) : Temperature = {
    val latIdx = gridLocation.lat + 89
    val lonIdx = gridLocation.lon + 180
    if (calculated(latIdx)(lonIdx)) cached(latIdx)(lonIdx)
    else {
      calculated(latIdx)(lonIdx) = true
      cached(latIdx)(lonIdx) = Visualization.predictTemperature(temperatures, Location(gridLocation.lat, gridLocation.lon))
      cached(latIdx)(lonIdx)
    }
  }
}
