package web.utils

import coloring.RGBColor
import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.filter.BlurFilter
import com.sksamuel.scrimage.nio.{ImageWriter, JpegWriter, PngWriter}
import com.sksamuel.scrimage.webp.WebpWriter
import web.guidelines.Dimension

import java.awt.Color
import java.io.File
import scala.util.Try

/** Functional utilities for image processing with safe IO using Either.
 *
 * All operations that involve reading or writing files return TransformationResult,
 * where Left(errorMessage) indicates failure and Right(file) indicates success.
 */
object ImageTransforms {

  private val supportedExtensions = Set("png", "jpg", "jpeg")
  case class TransformationError(message: String) extends AnyVal
  type TransformationResult = Either[TransformationError, File]

  sealed trait ImageFormat {
    def extension: String

    def writer: ImageWriter
  }

  case object Webp extends ImageFormat {
    val extension = ".webp"
    val writer: WebpWriter = WebpWriter.DEFAULT
  }

  case object Jpeg extends ImageFormat {
    val extension = ".jpeg"
    val writer: JpegWriter = JpegWriter.Default
  }

  case object Png extends ImageFormat {
    val extension = ".png"
    val writer: PngWriter = PngWriter.MinCompression
  }

  sealed trait ThumbType {
    def deviceName: String
  }

  case object Mobile extends ThumbType {
    val deviceName = "mobile"
  }

  case object Desktop extends ThumbType {
    val deviceName = "desktop"
  }

  /** List all supported image files in a folder.
   *
   * @param inputDir the folder to scan for image files
   * @return sequence of valid image files
   */
  def listImages(inputDir: File): Seq[File] =
    Option(inputDir.listFiles())
      .getOrElse(Array.empty)
      .filter(f => supportedExtensions.exists(ext => f.getName.toLowerCase.endsWith(ext)))

  //GENERATIVE

  /** Convert a single image to WebP safely.
   *
   * @param inputFile the image file to convert
   * @param outputDir folder to save the converted image
   * @param format    image writer
   * @return Either an error message or the output File
   */
  def convertTo(inputFile: File, outputDir: File, format: ImageFormat): TransformationResult =
    writeAs(inputFile, outputDir, format)

  /** Convert a list of images to WebP safely.
   *
   * @param inputFiles list of image files
   * @param outputDir  folder to save converted images
   * @param format     image writer
   * @return sequence of TransformationResult
   */
  def convertTo(inputFiles: Seq[File], outputDir: File, format: ImageFormat): Seq[TransformationResult] =
    inputFiles.map(file => convertTo(file, outputDir, format))

  /** Generate a thumbnail for a single image safely.
   *
   * @param inputFile the image file
   * @param outputDir folder to save thumbnail
   * @param thumbType "desktop" or "mobile"
   * @return Either an error message or the thumbnail File
   */
  def createThumbnail(inputFile: File, outputDir: File, thumbType: ThumbType): TransformationResult =
    Try {
      val image = ImmutableImage.loader().fromFile(inputFile)
      val scaled = thumbType.deviceName match {
        case "desktop" => image.scaleTo(300, 300)
        case "mobile" => image.scaleTo(150, 150)
      }
      val outputFile = new File(outputDir, s"$thumbType-${inputFile.getName}")
      scaled.output(WebpWriter.DEFAULT, outputFile)
      outputFile
    }.toEither.left.map(ex => TransformationError(s"Error creating thumbnail ${inputFile.getName}: ${ex.getMessage}"))

  /** Generate thumbnails for a list of images safely.
   *
   * @param inputFiles list of image files
   * @param outputDir  folder to save thumbnails
   * @param thumbType  "desktop" or "mobile"
   * @return sequence of TransformationResult
   */
  def createThumbnail(inputFiles: Seq[File], outputDir: File, thumbType: ThumbType): Seq[TransformationResult] =
    inputFiles.map(createThumbnail(_, outputDir, thumbType))

  /** Generates a series of placeholder images safely.
   *
   * @param number    the number of placeholder images to generate
   * @param width     width of each image
   * @param height    height of each image
   * @param fillColor optional Color to fill the image; if None, transparent
   * @param applyBlur whether to apply a blur effect
   * @param outputDir folder to write placeholder images
   * @return sequence of TransformationResult
   */
  def generatePlaceholders(
                            number: Int,
                            width: Int,
                            height: Int,
                            fillColor: Option[Color] = None,
                            applyBlur: Boolean = true,
                            outputDir: File
                          ): Seq[TransformationResult] =
    (1 to number).map { i =>
      Try {
        val image = fillColor.map(c => ImmutableImage.filled(width, height, c))
          .getOrElse(ImmutableImage.create(width, height))
        val finalImage = if (applyBlur) image.filter(new BlurFilter()) else image
        val outputFile = new File(outputDir, s"placeholder_$i.webp")
        finalImage.output(WebpWriter.DEFAULT, outputFile)
        outputFile
      }.toEither.left.map(ex => TransformationError(s"Error generating placeholder $i: ${ex.getMessage}"))
    }

  /** Removes all metadata from an image and saves it as a clean PNG safely.
   *
   * @param inputFile  the image file to process
   * @param outputFile destination file for stripped image
   * @return TransformationResult
   */
  def stripMetadata(inputFile: File, outputFile: File): TransformationResult =
    Try {
      val image = ImmutableImage.loader().fromFile(inputFile)
      image.output(PngWriter.MaxCompression, outputFile)
      outputFile
    }.toEither.left.map(ex => TransformationError(s"Error stripping metadata for ${inputFile.getName}: ${ex.getMessage}"))

  //TRANSFORMABLE to be refactored for pipes

  /** Automatically crops an image, optionally using a color as the background reference, safely.
   *
   * @param image   immutable image
   * @param bgColor optional Color used as background
   * @return TransformationResult
   */
  def autoCrop(image: ImmutableImage, bgColor: Option[Color] = None): ImmutableImage = {
    bgColor match {
      case Some(color) => image.autocrop(color)
      case _ => image
    }
  }

  /** Automatically crops an image, optionally using a color as the background reference, safely.
   *
   * @param inputFile  the input file to crop
   * @param outputFile the destination file
   * @param bgColor    optional Color used as background
   * @return TransformationResult
   */
  def autoCrop(inputFile: File, outputFile: File, bgColor: Option[Color]): TransformationResult =
    Try {
      val image = ImmutableImage.loader().fromFile(inputFile)
      val cropped = autoCrop(image, bgColor)
      cropped.output(WebpWriter.DEFAULT, outputFile)
      outputFile
    }.toEither.left.map(ex => TransformationError(s"Error auto-cropping ${inputFile.getName}: ${ex.getMessage}"))

  //2026
  sealed trait Axis

  case object Horizontal extends Axis

  case object Vertical extends Axis

  /**
   * If the source image is larger, it will be scaled down, maintaining aspect ratio.
   * If the source image is smaller, it will be returned unmodified.
   *
   * @param image input
   * @param dims  Dimension(wxh)
   * @return bounded image
   */
  def bound(image: ImmutableImage, dims: Dimension): ImmutableImage = image.bound(dims.width, dims.height)

  /**
   * Flips an image
   *
   * @param image input
   * @param axis  Horizontal|Vertical
   * @return flipped image
   */
  def flip(image: ImmutableImage, axis: Axis): ImmutableImage =
    getAxis(image, axis)

  /**
   * Adds a border
   *
   * @param image     input
   * @param rgb       color
   * @param thickness border thickness
   * @return border-padded image
   */
  def addBorder(
                 image: ImmutableImage,
                 rgb: RGBColor,
                 thickness: Int
               ): ImmutableImage =
    image.pad(thickness, rgb.color)

  /**
   * Scales an image
   *
   * @param image  input
   * @param factor as double
   * @return scaled image
   */
  def scale(image: ImmutableImage, factor: Double): ImmutableImage = image.scale(factor)

  /**
   * Close up
   *
   * @param image  input
   * @param factor as double
   * @return zoomed in image
   */
  def zoom(image: ImmutableImage, factor: Double): ImmutableImage = image.zoom(factor)

  //helper
  private def getAxis(image: ImmutableImage, axis: Axis): ImmutableImage = {
    axis match {
      case Horizontal => image.flipX()
      case Vertical => image.flipY()
    }
  }

  //for batch-single

  /**
   * If the source image is larger, it will be scaled down, maintaining aspect ratio.
   * If the source image is smaller, it will be returned unmodified.
   *
   * @param image     input
   * @param outputDir output path
   * @param dim       Dimension(wxh)
   * @return Either an error message or the output File
   */
  def bound(image: File, outputDir: File, dim: Dimension): TransformationResult =
    withImage(image, outputDir)(img => bound(img, dim),"Error bounding")

  /**
   * Flips an image
   *
   * @param image     input
   * @param outputDir output path
   * @param axis      Horizontal|Vertical
   * @return Either an error message or the output File
   */
  def flip( image: File, outputDir: File, axis: Axis): TransformationResult =
    withImage(image, outputDir)(img => getAxis(img, axis),"Error flipping")

  /**
   * Adds a border
   *
   * @param image     input
   * @param outputDir output path
   * @param rgb       color
   * @param thickness border thickness
   * @return Either an error message or the output File
   */
  def addBorder( image: File, outputDir: File, rgb: RGBColor,  thickness: Int): TransformationResult =
    withImage(image, outputDir)(img => addBorder(img, rgb, thickness),"Error adding border")

  /**
   * Scales an image
   *
   * @param image  input
   * @param factor as double
   * @return Either an error message or the output File
   */
  def scale( image: File,  outputDir: File,factor: Double): TransformationResult =
    withImage(image, outputDir)(_.scale(factor),"Error scaling")

  /**
   * Close up
   *
   * @param image  input
   * @param factor as double
   * @return Either an error message or the output File
   */
  def zoom( image: File, outputDir: File,factor: Double): TransformationResult =
    withImage(image, outputDir)(_.zoom(factor),"Error zooming in")

  //for batch multi
  /**
   * Takes a list of Files and adds a border to them
   *
   * @param inputFiles input
   * @param outputDir  output path
   * @param color      RGBColor
   * @param thickness  factor as double
   * @return Either Seq of error messages or the output files
   */
  def addBorder(
                 inputFiles: Seq[File],
                 outputDir: File,
                 color: RGBColor,
                 thickness: Int
               ): Seq[TransformationResult] =
    inputFiles.map(addBorder(_, outputDir, color, thickness))

  /**
   * Takes a list of Files and scales them
   *
   * @param inputFiles input
   * @param outputDir  output path
   * @param factor     factor as double
   * @return Either Seq of error messages or the output files
   */
  def scale(
             inputFiles: Seq[File],
             outputDir: File,
             factor: Double
           ): Seq[TransformationResult] =
    inputFiles.map(scale(_, outputDir, factor))

  /**
   * Takes a list of Files and zooms in them
   *
   * @param inputFiles input
   * @param outputDir  output path
   * @param factor     factor as double
   * @return Either Seq of error messages or the output files
   */
  def zoom(
            inputFiles: Seq[File],
            outputDir: File,
            factor: Double
          ): Seq[TransformationResult] =
    inputFiles.map(zoom(_, outputDir, factor))

  private def withImage(
                         input: File,
                         outputDir: File,
                         writer: ImageWriter = WebpWriter.DEFAULT
                       )(
                         transform: ImmutableImage => ImmutableImage,
                         errorCtx: String
                       ): TransformationResult =
    Try {
      val img = ImmutableImage.loader().fromFile(input)
      val result = transform(img)
      val outputFile = new File(outputDir, input.getName)
      result.output(writer, outputFile)
      outputFile
    }.toEither.left.map(ex =>
      TransformationError(s"$errorCtx ${input.getName}: ${ex.getMessage}")
    )

  private def writeAs(
                       input: File,
                       outputDir: File,
                       format: ImageFormat
                     ): TransformationResult =
    Try {
      val image = ImmutableImage.loader().fromFile(input)
      val outputFile =
        new File(outputDir, input.getName.replaceAll("\\.[^.]+$", format.extension))
      image.output(format.writer, outputFile)
      outputFile
    }.toEither.left.map(ex =>
      TransformationError(s"Error converting ${input.getName}: ${ex.getMessage}")
    )


}
