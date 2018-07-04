package observatory.utils;

import java.nio.file.Paths
import observatory.Location
import observatory.GridLocation
import scala.math._
/**
  * Created by Tuan on 2018/05/18.
  */
object Utils {

  val REarth = 6371

  def resourcePath(resource: String): String =
    Paths.get(getClass.getResource(resource).toURI).toString

  def greatCircleDistance(l1: Location, l2: Location): Double = {
    if (l1.lat == l2.lat && l1.lon == l2. lon)  0
    else if (l1.lat == -l2.lat && (l1.lon == l2.lon + 180 || l1.lon == l2.lon - 180)) REarth*Pi
    else {
      val l1Latrad = l1.lat.toRadians
      val l1Lonrad = l1.lon.toRadians
      val l2Latrad = l2.lat.toRadians
      val l2Lonrad = l2.lon.toRadians
      val deltaLonrad = abs(l1Lonrad - l2Lonrad)
      val deltaPhi = acos(sin(l1Latrad)* sin(l2Latrad)
        + cos(l1Latrad)*cos(l2Latrad)*cos(deltaLonrad))
      REarth*deltaPhi
    }
  }

  def greatCircleDistance2(l1: Location, l2: Location): Double = {
    if (l1.lat == l2.lat && l1.lon == l2. lon)  0
    else if (l1.lat == -l2.lat && (l1.lon == l2.lon + 180 || l1.lon == l2.lon - 180)) REarth*Pi
    else {
      val l1Latrad = l1.lat.toRadians
      val l1Lonrad = l1.lon.toRadians
      val l2Latrad = l2.lat.toRadians
      val l2Lonrad = l2.lon.toRadians
      val delta_lon = abs(l1Lonrad - l2Lonrad)
      val delta_lat = abs(l1Latrad - l2Latrad)
      val delta_sigma =   2 * asin(
        sqrt(
          pow(sin(delta_lat/2), 2)
          + cos(l1Latrad) * cos(l2Latrad)
          * pow(sin(delta_lon/2), 2)
        )
      )
      REarth * delta_sigma
    }
  }

  def greatCircleDistance3(l1: Location, l2: Location): Double = {
    val r = 6372.8
    val lat1 = toRadians(l1.lat)
    val lon1 = toRadians(l1.lon)
    val lat2 = toRadians(l2.lat)
    val lon2 = toRadians(l2.lon)
    val deltaLat = abs(l1.lat - l2.lat)
    val deltaLon = abs(l1.lon - l2.lon)
    val temp = pow(sin(deltaLat / 2), 2) + cos(lat1) * cos(lat2) * pow(sin(deltaLon / 2), 2)
    r * 2 * atan2(sqrt(temp), sqrt(1 - temp))
  }

  def locationToGridIndex(loc: Location): GridLocation = {
    val x = floor(loc.lon + 180).toInt;
    val y = floor(0 - loc.lat + 90).toInt
    val idX = if (x == 359) x - 1 else x
    val idY = if (y == 179) y - 1 else y
    new GridLocation(90 - idY, idX - 180)
  }
}
