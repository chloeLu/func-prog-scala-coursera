
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

  def partition(width: Int, numTasks: Int) = {
    val w: Double = width.asInstanceOf[Double]
    val step: Int = math.ceil(w / numTasks).asInstanceOf[Int]
    val froms = 0 until width by step
    val tos = froms map (x => if (x + step < width) x + step else width)
    froms zip tos
  }

  /** Image is a two-dimensional matrix of pixel values. */
  class Img(val width: Int, val height: Int, private val data: Array[RGBA]) {
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))

    def apply(x: Int, y: Int): RGBA = data(y * width + x)

    def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c
  }

  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {

    val allPixels = (for {
      xx <- clamp(x - radius, 0, src.width - 1) to clamp(x + radius, 0, src.width - 1)
      yy <- clamp(y - radius, 0, src.height - 1) to clamp(y + radius, 0, src.height - 1)
      if !(xx == x && yy == y)
    } yield src(xx, yy)) :+ src(x, y)

    val zero = (0, 0, 0, 0)
    val numPixels = allPixels.length
    val (r, g, b, a) = allPixels.foldLeft(zero)(
      (res, p) => (res._1 + red(p), res._2 + green(p), res._3 + blue(p), res._4 + alpha(p))
    )

    rgba(r / numPixels, g / numPixels, b / numPixels, a / numPixels)
  }

}
