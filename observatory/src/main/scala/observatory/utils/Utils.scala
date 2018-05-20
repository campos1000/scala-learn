package observatory.utils;

import java.nio.file.Paths
/**
  * Created by Tuan on 2018/05/18.
  */
object Utils {

  def resourcePath(resource: String): String =
    Paths.get(getClass.getResource(resource).toURI).toString
}
