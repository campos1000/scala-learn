package observatory

import java.nio.file.Paths
import java.time.LocalDate
import org.apache.spark.sql._
import org.apache.spark.sql.functions._
import org.apache.spark.sql.types._
/**
  * 1st milestone: data extraction
  */
object Extraction {
  import org.apache.log4j.{Level, Logger}
  Logger.getLogger("org.apache.spark").setLevel(Level.WARN)
  val spark: SparkSession =
    SparkSession
      .builder()
      .appName("DataExtraction")
      .config("spark.master", "local")
      .getOrCreate()

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] = {
    //val pathFile = Paths.get(getClass.getResource(stationsFile).toURI).toString
    val pathFile = "resoures/"
    val stationLines = spark.sparkContext.textFile(pathFile)
    val stations = stationLines.map(line => line.split(",")).filter(arr => arr.length == 4)
      .map(arr => ((arr(0), if(arr(1).isEmpty) "-1" else arr(1)), Location(arr(2).toDouble, arr(3).toDouble))).persist()
    val temperaturesPath = Paths.get(getClass.getResource(temperaturesFile).toURI).toString
    val temperatureLines = spark.sparkContext.textFile(temperaturesPath)
    val temperature = temperatureLines.map(line => line.split(","))
      .map(arr => ((arr(0), if(arr(1).isEmpty) "-1" else arr(1)), (LocalDate.of(year, arr(2).toInt, arr(3).toInt), arr(4).toDouble))).persist()
    val groupRecords = stations.join(temperature).map{
      case ((address1, address2), (location, (localDate, temp))) => (localDate, location, temp)
    }.persist()
    stations.unpersist();
    temperature.unpersist();
    groupRecords.collect()
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Temperature)]): Iterable[(Location, Temperature)] = {
    val recordRdds = spark.sparkContext.parallelize(records.toList)
    recordRdds.map{case (localDate, location, temperature) => (location, temperature)}.mapValues(temp => (temp, 1))
      .reduceByKey((value1, value2) => (value1._1 + value2._1, value1._2 + value2._2)).mapValues((value) => value._1/value._2).collect()
  }
}
