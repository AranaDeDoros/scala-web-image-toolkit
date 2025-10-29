package web.utils

import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.angles.Radians
import com.sksamuel.scrimage.filter.{BlurFilter, GrayscaleFilter}
import com.sksamuel.scrimage.nio.{JpegWriter, PngWriter}
import com.sksamuel.scrimage.webp.WebpWriter

import java.awt.Color
import java.io.File
import scala.util.Try

/** Functional utilities for image processing with safe IO using Either.
 *
 * All operations that involve reading or writing files return Either[String, File],
 * where Left(errorMessage) indicates failure and Right(file) indicates success.
 */
object Utils {

  private val supportedExts = Set("png", "jpg", "jpeg", "gif", "bmp")

  /** List all supported image files in a folder.
   *
   * @param inputDir the folder to scan for image files
   * @return sequence of valid image files
   */
  def listImages(inputDir: File): Seq[File] =
    Option(inputDir.listFiles())
      .getOrElse(Array.empty)
      .filter(f => supportedExts.exists(ext => f.getName.toLowerCase.endsWith(ext)))

  /** Convert a single image to WebP safely.
   *
   * @param inputFile the image file to convert
   * @param outputDir folder to save the converted image
   * @return Either an error message or the output File
   */
  def convertToWebp(inputFile: File, outputDir: File): Either[String, File] =
    Try {
      val image = ImmutableImage.loader().fromFile(inputFile)
      val outputFile = new File(outputDir, inputFile.getName.replaceAll("\\.[^.]+$", ".webp"))
      image.output(WebpWriter.MAX_LOSSLESS_COMPRESSION, outputFile)
      outputFile
    }.toEither.left.map(ex => s"Error converting ${inputFile.getName}: ${ex.getMessage}")

  /** Convert a list of images to WebP safely.
   *
   * @param inputFiles list of image files
   * @param outputDir  folder to save converted images
   * @return sequence of Either[String, File]
   */
  def convertToWebp(inputFiles: Seq[File], outputDir: File): Seq[Either[String, File]] =
    inputFiles.map(convertToWebp(_, outputDir))

  /** Generate a thumbnail for a single image safely.
   *
   * @param inputFile the image file
   * @param outputDir folder to save thumbnail
   * @param thumbType "desktop" or "mobile"
   * @return Either an error message or the thumbnail File
   */
  def createThumbnail(inputFile: File, outputDir: File, thumbType: String): Either[String, File] =
    Try {
      val image = ImmutableImage.loader().fromFile(inputFile)
      val scaled = thumbType match {
        case "desktop" => image.scaleTo(300, 300)
        case "mobile" => image.scaleTo(150, 150)
      }
      val outputFile = new File(outputDir, s"$thumbType-${inputFile.getName}")
      scaled.output(WebpWriter.DEFAULT, outputFile)
      outputFile
    }.toEither.left.map(ex => s"Error creating thumbnail ${inputFile.getName}: ${ex.getMessage}")

  /** Generate thumbnails for a list of images safely.
   *
   * @param inputFiles list of image files
   * @param outputDir  folder to save thumbnails
   * @param thumbType  "desktop" or "mobile"
   * @return sequence of Either[String, File]
   */
  def createThumbnail(inputFiles: Seq[File], outputDir: File, thumbType: String): Seq[Either[String, File]] =
    inputFiles.map(createThumbnail(_, outputDir, thumbType))

  /** Generates a series of placeholder images safely.
   *
   * @param number    the number of placeholder images to generate
   * @param width     width of each image
   * @param height    height of each image
   * @param fillColor optional Color to fill the image; if None, transparent
   * @param applyBlur whether to apply a blur effect
   * @param outputDir folder to write placeholder images
   * @return sequence of Either[String, File]
   */
  def generatePlaceholders(
                            number: Int,
                            width: Int,
                            height: Int,
                            fillColor: Option[Color] = None,
                            applyBlur: Boolean = true,
                            outputDir: File
                          ): Seq[Either[String, File]] =
    (1 to number).map { i =>
      Try {
        val image = fillColor.map(c => ImmutableImage.filled(width, height, c))
          .getOrElse(ImmutableImage.create(width, height))
        val finalImage = if (applyBlur) image.filter(new BlurFilter()) else image
        val outputFile = new File(outputDir, s"placeholder_$i.webp")
        finalImage.output(WebpWriter.MAX_LOSSLESS_COMPRESSION, outputFile)
        outputFile
      }.toEither.left.map(ex => s"Error generating placeholder $i: ${ex.getMessage}")
    }

  /** Removes all metadata from an image and saves it as a clean PNG safely.
   *
   * @param inputFile  the image file to process
   * @param outputFile destination file for stripped image
   * @return Either[String, File]
   */
  def stripMetadata(inputFile: File, outputFile: File): Either[String, File] =
    Try {
      val image = ImmutableImage.loader().fromFile(inputFile)
      image.output(PngWriter.MaxCompression, outputFile)
      outputFile
    }.toEither.left.map(ex => s"Error stripping metadata for ${inputFile.getName}: ${ex.getMessage}")

  /** Automatically crops an image, optionally using a color as the background reference, safely.
   *
   * @param inputFile  the input file to crop
   * @param outputFile the destination file
   * @param bgColor    optional Color used as background
   * @return Either[String, File]
   */
  def autoCrop(inputFile: File, outputFile: File, bgColor: Option[Color] = None): Either[String, File] =
    Try {
      val image = ImmutableImage.loader().fromFile(inputFile)
      val cropped = bgColor.map(c => image.autocrop(c)).getOrElse(image)
      cropped.output(WebpWriter.MAX_LOSSLESS_COMPRESSION, outputFile)
      outputFile
    }.toEither.left.map(ex => s"Error auto-cropping ${inputFile.getName}: ${ex.getMessage}")

  /** Utility object for performing image preprocessing tasks for OCR.
   *
   * This includes rotation correction, grayscale conversion, contrast adjustment,
   * and optional binarization. All functions that manipulate pixels are pure and
   * do not perform IO.
   */
  object OCR {
    /** Clamps a color channel value to the valid RGB range [0, 255].
     *
     * @param v the input channel value
     * @return the clamped value within [0,255]
     */
    private val clamp: PartialFunction[Int, Int] = {
      case x if x < 0   => 0
      case x if x > 255 => 255
      case x            => x
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
        case 1.0        => Normal
        case f if f > 1 => High(f)
        case f if f < 1 => Low(f)
      }
    }
    /** Adjusts image contrast by scaling the distance of pixel intensities from
     * the midpoint (128).
     *
     * @param image  the input image
     * @param factor contrast factor (1.0 = normal, >1.0 = higher contrast, <1.0 = lower contrast)
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
     * @param image    the input image
     * @param radians rotation angle in radians (positive = counterclockwise)
     * @return a new [[ImmutableImage]] rotated by the specified angle
     */
    def rotate(image: ImmutableImage, radians: Double): ImmutableImage =
      if (radians != 0.0) image.rotate(new Radians(radians)) else image

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

    /** Performs a complete preprocessing pipeline for OCR:
     * rotation correction, grayscale conversion, contrast adjustment,
     * and optional binarization.
     *
     * @param image          the input image
     * @param tilt           rotation angle in degrees (default: 0.0)
     * @param contrastFactor contrast multiplier (default: 1.4)
     * @param threshold      threshold for binarization (default: 128)
     * @param doBinarize     whether to perform binarization (default: true)
     * @return the processed [[ImmutableImage]] ready for OCR
     */
    def prepareOCR(
                    image: ImmutableImage,
                    tilt: Double = 0.0,
                    contrastFactor: Double = 1.4,
                    threshold: Int = 128,
                    doBinarize: Boolean = true
                  ): ImmutableImage = {
      val rotated = rotate(image, tilt)
      val gray = grayscale(rotated)
      val factor = ContrastLevel.fromFactor(contrastFactor)
      val contrasted = contrast(gray, factor)
      if (doBinarize) binarize(contrasted, threshold) else contrasted
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


}
