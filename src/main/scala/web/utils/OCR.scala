package web.utils

import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.angles.Radians
import com.sksamuel.scrimage.filter.GrayscaleFilter
import com.sksamuel.scrimage.nio.JpegWriter
import web.utils.ImageTransforms.TransformationError

import java.awt.Color
import java.io.File
import scala.util.Try

case class OptimizationError(message: String) extends AnyVal

/** Utility object for performing image preprocessing tasks for OCR.
 *
 * This includes rotation correction, grayscale conversion, contrast adjustment,
 * and optional binarization. All functions that manipulate pixels are pure and
 * do not perform IO.
 */
object OCR {
  type  OptimizationBytesResult = Either[ OptimizationError, Array[Byte]]
  /**
   * Clamps an integer value to the range [0, 255].
   *
   * This is a `PartialFunction` from `Int` to `Int` that:
   *   - Returns 0 if the input is less than 0.
   *   - Returns 255 if the input is greater than 255.
   *   - Returns the input itself if it is within the range 0 to 255.
   *
   * Example usage:
   * {{{
   *   clamp(-5)   // returns 0
   *   clamp(100)  // returns 100
   *   clamp(300)  // returns 255
   * }}}
   */
  private val clamp: PartialFunction[Int, Int] = {
    case x if x < 0 => 0
    case x if x > 255 => 255
    case x => x
  }

  sealed trait ContrastLevel {
    def factor: Double
  }

  object ContrastLevel {
    /** (factor = 1.0) */
    case object Normal extends ContrastLevel {
      val factor: Double = 1.0
    }

    /** (factor > 1.0) */
    case class High(factor: Double) extends ContrastLevel {
      require(factor > 1.0, "High contrast factor must be > 1.0")
    }

    /** (factor < 1.0) */
    case class Low(factor: Double) extends ContrastLevel {
      require(factor < 1.0, "Low contrast factor must be < 1.0")
    }

    /** factory method from a Double factor */
    def fromFactor(factor: Double): ContrastLevel = factor match {
      case 1.0 => Normal
      case f if f > 1 => High(f)
      case f if f < 1 => Low(f)
    }
  }

  /** Converts an image to grayscale using a weighted luminance filter.
   *
   * @param image the input image
   * @return a new grayscale [[ImmutableImage]]
   */
  def grayscale(image: ImmutableImage): ImmutableImage =
    image.filter(new GrayscaleFilter())


  /** Binarizes an image by converting all pixels to either black or white
   * based on a brightness threshold.
   *
   * @param image     the input image
   * @param threshold cutoff intensity (0–255) to determine black or white (default: 128)
   * @return a new binary (black-and-white) [[ImmutableImage]]
   */
  def binarize(image: ImmutableImage, threshold: Int = 128): ImmutableImage =
    image.map { p =>
      val v = if ((p.red + p.green + p.blue) / 3 > threshold) 255 else 0
      new Color(v, v, v, p.alpha)
    }

  /** Adjusts image contrast by scaling the distance of pixel intensities from
   * the midpoint (128).
   *
   * @param image  the input image
   * @param level contrast level (1.0 = normal, >1.0 = higher contrast, <1.0 = lower contrast)
   * @return a new [[ImmutableImage]] with adjusted contrast
   */
  def contrast(image: ImmutableImage, level: ContrastLevel): ImmutableImage = {
    val factor = level.factor
    image.map { p =>
      def adj(c: Int) = clamp(((c - 128) * factor + 128).toInt)
      new java.awt.Color(adj(p.red), adj(p.green), adj(p.blue), p.alpha)
    }
  }

  /** Rotates an image to correct tilt or skew.
   *
   * @param image   the input image
   * @param radians rotation angle in radians (positive = counterclockwise)
   * @return a new [[ImmutableImage]] rotated by the specified angle
   */
  def rotate(image: ImmutableImage, radians: Double): ImmutableImage =
    if (radians != 0.0) image.rotate(new Radians(radians)) else image


  /** Performs a complete preprocessing pipeline for OCR:
   * rotation correction, grayscale conversion, contrast adjustment,
   * and optional binarization.
   *
   * @param input          the input image
   * @param tilt           rotation angle in degrees (default: 0.0)
   * @param contrastFactor contrast multiplier (default: 1.4)
   * @param threshold      threshold for binarization (default: 128)
   * @param doBinarize     whether to perform binarization (default: true)
   * @return the processed [[ImmutableImage]] ready for OCR
   */
  def optimize(
                input: ImmutableImage,
                tilt: Double ,
                contrastFactor: Double ,
                threshold: Int ,
                doBinarize: Boolean
              ): ImmutableImage = {
    val rotated = rotate(input, tilt)
    val gray = grayscale(rotated)
    val factor = ContrastLevel.fromFactor(contrastFactor)
    val contrasted = contrast(gray, factor)
    if (doBinarize) binarize(contrasted, threshold) else contrasted
  }

  /** Performs a complete preprocessing pipeline for OCR:
   * rotation correction, grayscale conversion, contrast adjustment,
   * and optional binarization.
   *
   * @param input          the input images
   * @param tilt           rotation angle in degrees (default: 0.0)
   * @param contrastFactor contrast multiplier (default: 1.4)
   * @param threshold      threshold for binarization (default: 128)
   * @param doBinarize     whether to perform binarization (default: true)
   * @return the processed [Seq[[ImmutableImage]]] ready for OCR
   */
  def optimize(
                input: Seq[ImmutableImage],
                tilt: Double,
                contrastFactor: Double ,
                threshold: Int,
                doBinarize: Boolean
              ): Seq[ImmutableImage] = {
    input.map{
      img => {
        val rotated = rotate(img, tilt)
        val gray = grayscale(rotated)
        val factor = ContrastLevel.fromFactor(contrastFactor)
        val contrasted = contrast(gray, factor)
        if (doBinarize) binarize(contrasted, threshold) else contrasted
      }
    }
  }

  /** Performs a complete preprocessing pipeline for OCR:
   * rotation correction, grayscale conversion, contrast adjustment,
   * and optional binarization.
   *
   * @param input          the file bytes
   * @param tilt           rotation angle in degrees (default: 0.0)
   * @param contrastFactor contrast multiplier (default: 1.4)
   * @param threshold      threshold for binarization (default: 128)
   * @param doBinarize     whether to perform binarization (default: true)
   * @return                Either[OptimizationError, Array[Byte]
   */
  def optimize(
                input: Array[Byte],
                tilt: Double,
                contrastFactor: Double,
                threshold: Int,
                doBinarize: Boolean
              ): OptimizationBytesResult = {

    Try {
      val image = ImmutableImage.loader().fromBytes(input)
      val rotated = rotate(image, tilt)
      val gray = grayscale(rotated)
      val factor = ContrastLevel.fromFactor(contrastFactor)
      val contrasted = contrast(gray, factor)
      val result = if (doBinarize) binarize(contrasted, threshold) else contrasted
      result.bytes(new JpegWriter().withCompression(90))
    }.toEither.left.map(ex =>
      OptimizationError(s"Error converting bytes: ${ex.getMessage}")
    )

  }

  /** Saves an image to disk in JPEG format with adjustable compression quality.
   *
   * @param image   the image to save
   * @param path    destination file path
   * @param quality compression quality (0–100, default: 90)
   * @return Either[String, File] with the output file or error message
   */
  def saveAsJpeg(image: ImmutableImage, path: String, quality: Int = 90): Either[String, File] =
    Try {
      val outFile = new File(path)
      image.output(new JpegWriter().withCompression(quality), outFile)
      outFile
    }.toEither.left.map(ex => s"Error saving JPEG to $path: ${ex.getMessage}")

}
