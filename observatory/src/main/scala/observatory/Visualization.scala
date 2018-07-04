package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import observatory.utils.{Utils => ObUtils}


/**
  * 2nd milestone: basic visualization
  */
object Visualization {
  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {
    //val temperatureRdds = temperatures.toList.par
    val distanceRdds = temperatures.map{case (loc, temp) => {
      (ObUtils.greatCircleDistance(loc, location), temp)
    }}
    val nearLoc = distanceRdds.filter{case (loc, temp) => loc < 1}
    if (nearLoc.nonEmpty) {
      nearLoc.minBy(_._1)._2
    }
    else {
      val aggregateTemp = distanceRdds.map{case (distance, temp) => {
          (1/(distance*distance), temp/(distance*distance))
      }}.reduce[(Double, Double)]{ case ((accA, accB), (a, b)) => (accA + a, accB + b) }
      aggregateTemp._2/aggregateTemp._1
    }
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color = {
    val pointPar = points
    val maxPoint = pointPar.maxBy(_._1)
    val minPoint = pointPar.minBy(_._1)

    if (value >= maxPoint._1) maxPoint._2

    //if value is between in points
    else if (value > minPoint._1) {
      val pointAbove = findPointAbove(pointPar, value)
      val pointBelow = findPointBelow(pointPar, value)
      if (pointBelow._1 == value)  pointBelow._2
      else if (pointAbove._1 == value)  pointAbove._2
      else calInterpolate(pointBelow, pointAbove, value)
    }

    else minPoint._2
  }

  def calInterpolate(point1: (Temperature, Color), point2: (Temperature, Color), value: Temperature):Color = {
    val temper1 = point1._1;
    val color1 = point1._2;
    val temper2 = point2._1;
    val color2 = point2._2;
    val red = (color1.red + (value - temper1)*(color2.red - color1.red)/(temper2 - temper1)).round.toInt;
    val blue = (color1.blue + (value - temper1)*(color2.blue - color1.blue)/(temper2 - temper1)).round.toInt;
    val green = (color1.green + (value - temper1)*(color2.green - color1.green)/(temper2 - temper1)).round.toInt;
    new Color(red, green, blue)
  }

  def findPointAbove (points: Iterable[(Temperature, Color)], value: Temperature): (Temperature, Color) = {
    points.reduce((point1, point2) => {
      if (point1._1 <= point2._1) {
        if (point1._1 >= value ) point1
        else point2
      } else {
        if (point2._1 >= value) point2
        else point1
      }
    })
  }

  def findPointBelow (points: Iterable[(Temperature, Color)], value: Temperature): (Temperature, Color) = {
    points.reduce((point1, point2) => {
      if (point1._1 <= point2._1) {
        if (point2._1 <= value ) point2
        else point1
      } else {
        if (point1._1 <= value) point1
        else point2
      }
    })
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): Image = {
    val locationList = for {
      latIndex <- 90 to -89 by -1
      longIndex <- -180 to 179
    } yield Location(latIndex, longIndex)

    val pixels = locationList.par.map(loc => {
      val color = interpolateColor(colors, predictTemperature(temperatures, loc))
      Pixel(color.red, color.green, color.blue, 255)
    }).toArray

    Image(360, 180, pixels)
  }

}

