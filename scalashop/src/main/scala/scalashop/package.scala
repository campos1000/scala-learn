
import common._

package object scalashop {

  /** The value of every pixel is represented as a 32 bit integer. */
  type RGBA = Int

  /** Returns the red component. */
  def red(c: RGBA): Int = (0xff000000 & c) >>> 24

  /** Returns the green component. */
  def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

  /** Returns the blue component. */
  def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

  /** Returns the alpha component. */
  def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

  /** Used to create an RGBA value from separate components. */
  def rgba(r: Int, g: Int, b: Int, a: Int): RGBA = {
    (r << 24) | (g << 16) | (b << 8) | (a << 0)
  }

  /** Restricts the integer into the specified range. */
  def clamp(v: Int, min: Int, max: Int): Int = {
    if (v < min) min
    else if (v > max) max
    else v
  }

  /** Image is a two-dimensional matrix of pixel values. */
  class Img(val width: Int, val height: Int, private val data: Array[RGBA]) {
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))
    def apply(x: Int, y: Int): RGBA = data(y * width + x)
    def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c
  }

  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    val listRawPoint = for {
      i <- x - radius to x + radius
      j <- y - radius to y + radius
    } yield (i, j)

    val listPoint = listRawPoint.filter{case (_1, _2) => _1 >=0 && _1 < src.width && _2 >= 0 && _2 < src.height }
    val listPointImg = listPoint.map{case (_1, _2) => src(_1, _2)}
    val sumRed = listPointImg.foldLeft(0)(_ + red(_))
    val sumGreen = listPointImg.foldLeft(0)(_ + green(_))
    val sumBlue = listPointImg.foldLeft(0)(_ + blue(_))
    val sumAlpha = listPointImg.foldLeft(0)(_ + alpha(_))
    rgba(sumRed/listPoint.size, sumGreen/listPoint.size, sumBlue/listPoint.size, sumAlpha/listPoint.size)
  }

  def generateTaskList(length: Int, numTasks: Int): List[(Int, Int)] = {
    if (numTasks >= length) {
      val x = (length to 0 by -1).toList
      return x.zip(x.tail)
    } else {
      if (length % numTasks == 0) {
        val x = (length to 0 by -(length/numTasks)).toList
        return x.zip(x.tail)
      } else {
        val newWidth = (length/numTasks)*numTasks
        val x = (newWidth to 0 by -(newWidth/numTasks)).toList
        val temp = x.zip(x.tail)
        return (length, temp.head._2) :: temp.tail
      }
    }
  }
}
