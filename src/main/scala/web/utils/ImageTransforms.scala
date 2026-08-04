package web.utils

import coloring.RGBColor
import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.angles.Radians
import com.sksamuel.scrimage.filter.{BlurFilter, GrayscaleFilter}
import com.sksamuel.scrimage.nio.{ImageWriter, JpegWriter, PngWriter}
import com.sksamuel.scrimage.webp.WebpWriter
import de.androidpit.colorthief.ColorThief
import web.guidelines.Dimension

import java.awt.image.BufferedImage
import java.awt.{Color, Font, RenderingHints}
import java.io.{File, InputStream}
import javax.imageio.ImageIO
import scala.util.Try

/** Functional utilities for image processing with safe IO using Either.
 *
 * All operations that involve reading or writing files return TransformationResult,
 * where Left(errorMessage) indicates failure and Right(file) indicates success.
 *
 * Byte-array and InputStream variants are provided alongside the File-based ones
 * for use in API contexts, where images arrive as raw bytes/streams and results
 * are returned in-memory rather than written to disk.
 */
object ImageTransforms {

  private val supportedExtensions = Set("png", "jpg", "jpeg")
  case class TransformationError(message: String) extends AnyVal
  type TransformationResult = Either[TransformationError, File]

  /** Result type for the byte/stream-based variants: Left(error) or Right(encoded bytes). */
  type TransformationBytesResult = Either[TransformationError, Array[Byte]]

  sealed trait ImageFormat {
    def extension: String

    def writer: ImageWriter
  }

  case object Webp extends ImageFormat {
    val extension = ".webp"
    val writer: WebpWriter = WebpWriter.MAX_LOSSLESS_COMPRESSION
  }

  case object Jpeg extends ImageFormat {
    val extension = ".jpeg"
    val writer: JpegWriter = JpegWriter.Default
  }

  case object Png extends ImageFormat {
    val extension = ".png"
    val writer: PngWriter = PngWriter.NoCompression
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

  /** Convert a single image (as bytes) to the given format safely.
   *
   * @param inputBytes the raw image bytes
   * @param format     image writer
   * @return Either an error message or the encoded bytes
   */
  def convertTo(inputBytes: Array[Byte], format: ImageFormat): TransformationBytesResult =
    writeAsBytes(inputBytes, format)

  /** Convert a single image (as a stream) to the given format safely.
   *
   * @param inputStream the image input stream
   * @param format      image writer
   * @return Either an error message or the encoded bytes
   */
  def convertTo(inputStream: InputStream, format: ImageFormat): TransformationBytesResult =
    writeAsStream(inputStream, format)

  /** Convert a list of images to WebP safely.
   *
   * @param inputFiles list of image files
   * @param outputDir  folder to save converted images
   * @param format     image writer
   * @return sequence of TransformationResult
   */
  def convertTo(inputFiles: Seq[File], outputDir: File, format: ImageFormat): Seq[TransformationResult] =
    inputFiles.map(file => convertTo(file, outputDir, format))

  /** Convert a list of images (as bytes) to the given format safely.
   *
   * @param inputBytesSeq list of raw image byte arrays
   * @param format        image writer
   * @return sequence of TransformationBytesResult
   */
  def convertTo(inputBytesSeq: Seq[Array[Byte]], format: ImageFormat): Seq[TransformationBytesResult] =
    inputBytesSeq.map(bytes => convertTo(bytes, format))

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
      val scaled = toggleDesktopAndMobile(thumbType, image)
      val outputFile = new File(outputDir, s"$thumbType-${inputFile.getName}")
      scaled.output(WebpWriter.DEFAULT, outputFile)
      outputFile
    }.toEither.left.map(ex => TransformationError(s"Error creating thumbnail ${inputFile.getName}: ${ex.getMessage}"))

  /** Generate a thumbnail for a single image (as bytes) safely.
   *
   * @param inputBytes the raw image bytes
   * @param thumbType  "desktop" or "mobile"
   * @return Either an error message or the encoded thumbnail bytes
   */
  def createThumbnail(inputBytes: Array[Byte], thumbType: ThumbType): TransformationBytesResult =
    Try {
      val image = ImmutableImage.loader().fromBytes(inputBytes)
      val scaled = toggleDesktopAndMobile(thumbType, image)
      scaled.bytes(WebpWriter.DEFAULT)
    }.toEither.left.map(ex => TransformationError(s"Error creating thumbnail from bytes: ${ex.getMessage}"))

  /** Generate a thumbnail for a single image (as a stream) safely.
   *
   * @param inputStream the image input stream
   * @param thumbType   "desktop" or "mobile"
   * @return Either an error message or the encoded thumbnail bytes
   */
  def createThumbnail(inputStream: InputStream, thumbType: ThumbType): TransformationBytesResult =
    Try {
      val image = ImmutableImage.loader().fromStream(inputStream)
      val scaled = toggleDesktopAndMobile(thumbType, image)
      scaled.bytes(WebpWriter.DEFAULT)
    }.toEither.left.map(ex => TransformationError(s"Error creating thumbnail from stream: ${ex.getMessage}"))

  /** Generate thumbnails for a list of images safely.
   *
   * @param inputFiles list of image files
   * @param outputDir  folder to save thumbnails
   * @param thumbType  "desktop" or "mobile"
   * @return sequence of TransformationResult
   */
  def createThumbnail(inputFiles: Seq[File], outputDir: File, thumbType: ThumbType): Seq[TransformationResult] =
    inputFiles.map(createThumbnail(_, outputDir, thumbType))

  /** Generate thumbnails for a list of images (as bytes) safely.
   *
   * @param inputBytesSeq list of raw image byte arrays
   * @param thumbType     "desktop" or "mobile"
   * @return sequence of TransformationBytesResult
   */
  def createThumbnail(inputBytesSeq: Seq[Array[Byte]], thumbType: ThumbType): Seq[TransformationBytesResult] =
    inputBytesSeq.map(createThumbnail(_, thumbType))

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
        finalImage.output(WebpWriter.MAX_LOSSLESS_COMPRESSION, outputFile)
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

  /** Removes all metadata from an image (as bytes) and re-encodes it as a clean PNG safely.
   *
   * @param inputBytes the raw image bytes to process
   * @return Either an error message or the stripped PNG bytes
   */
  def stripMetadata(inputBytes: Array[Byte]): TransformationBytesResult =
    Try {
      val image = ImmutableImage.loader().fromBytes(inputBytes)
      image.bytes(PngWriter.MaxCompression)
    }.toEither.left.map(ex => TransformationError(s"Error stripping metadata from bytes: ${ex.getMessage}"))

  /** Removes all metadata from an image (as a stream) and re-encodes it as a clean PNG safely.
   *
   * @param inputStream the image input stream to process
   * @return Either an error message or the stripped PNG bytes
   */
  def stripMetadata(inputStream: InputStream): TransformationBytesResult =
    Try {
      val image = ImmutableImage.loader().fromStream(inputStream)
      image.bytes(PngWriter.MaxCompression)
    }.toEither.left.map(ex => TransformationError(s"Error stripping metadata from stream: ${ex.getMessage}"))

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
      cropped.output(WebpWriter.MAX_LOSSLESS_COMPRESSION, outputFile)
      outputFile
    }.toEither.left.map(ex => TransformationError(s"Error auto-cropping ${inputFile.getName}: ${ex.getMessage}"))

  /** Automatically crops an image (as bytes), optionally using a color as the background reference, safely.
   *
   * @param inputBytes the input image bytes to crop
   * @param bgColor    optional Color used as background
   * @return Either an error message or the cropped, encoded bytes
   */
  def autoCrop(inputBytes: Array[Byte], bgColor: Option[Color]): TransformationBytesResult =
    Try {
      val image = ImmutableImage.loader().fromBytes(inputBytes)
      val cropped = autoCrop(image, bgColor)
      cropped.bytes(WebpWriter.MAX_LOSSLESS_COMPRESSION)
    }.toEither.left.map(ex => TransformationError(s"Error auto-cropping bytes: ${ex.getMessage}"))

  /** Automatically crops an image (as a stream), optionally using a color as the background reference, safely.
   *
   * @param inputStream the input image stream to crop
   * @param bgColor     optional Color used as background
   * @return Either an error message or the cropped, encoded bytes
   */
  def autoCrop(inputStream: InputStream, bgColor: Option[Color]): TransformationBytesResult =
    Try {
      val image = ImmutableImage.loader().fromStream(inputStream)
      val cropped = autoCrop(image, bgColor)
      cropped.bytes(WebpWriter.MAX_LOSSLESS_COMPRESSION)
    }.toEither.left.map(ex => TransformationError(s"Error auto-cropping stream: ${ex.getMessage}"))

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
   * If the source image is larger, it will be scaled down, maintaining aspect ratio.
   * If the source image is smaller, it will be returned unmodified.
   *
   * @param inputBytes input image bytes
   * @param dim        Dimension(wxh)
   * @return Either an error message or the encoded bytes
   */
  def bound(inputBytes: Array[Byte], dim: Dimension): TransformationBytesResult =
    withImageBytes(inputBytes)(img => bound(img, dim), "Error bounding bytes")

  /**
   * If the source image is larger, it will be scaled down, maintaining aspect ratio.
   * If the source image is smaller, it will be returned unmodified.
   *
   * @param inputStream input image stream
   * @param dim         Dimension(wxh)
   * @return Either an error message or the encoded bytes
   */
  def bound(inputStream: InputStream, dim: Dimension): TransformationBytesResult =
    withImageStream(inputStream)(img => bound(img, dim), "Error bounding stream")

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
   * Flips an image (as bytes)
   *
   * @param inputBytes input image bytes
   * @param axis       Horizontal|Vertical
   * @return Either an error message or the encoded bytes
   */
  def flip(inputBytes: Array[Byte], axis: Axis): TransformationBytesResult =
    withImageBytes(inputBytes)(img => getAxis(img, axis), "Error flipping bytes")

  /**
   * Flips an image (as a stream)
   *
   * @param inputStream input image stream
   * @param axis        Horizontal|Vertical
   * @return Either an error message or the encoded bytes
   */
  def flip(inputStream: InputStream, axis: Axis): TransformationBytesResult =
    withImageStream(inputStream)(img => getAxis(img, axis), "Error flipping stream")

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
   * Adds a border (as bytes)
   *
   * @param inputBytes input image bytes
   * @param rgb        color
   * @param thickness  border thickness
   * @return Either an error message or the encoded bytes
   */
  def addBorder(inputBytes: Array[Byte], rgb: RGBColor, thickness: Int): TransformationBytesResult =
    withImageBytes(inputBytes)(img => addBorder(img, rgb, thickness), "Error adding border to bytes")

  /**
   * Adds a border (as a stream)
   *
   * @param inputStream input image stream
   * @param rgb         color
   * @param thickness   border thickness
   * @return Either an error message or the encoded bytes
   */
  def addBorder(inputStream: InputStream, rgb: RGBColor, thickness: Int): TransformationBytesResult =
    withImageStream(inputStream)(img => addBorder(img, rgb, thickness), "Error adding border to stream")

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
   * Scales an image (as bytes)
   *
   * @param inputBytes input image bytes
   * @param factor     as double
   * @return Either an error message or the encoded bytes
   */
  def scale(inputBytes: Array[Byte], factor: Double): TransformationBytesResult =
    withImageBytes(inputBytes)(_.scale(factor), "Error scaling bytes")

  /**
   * Scales an image (as a stream)
   *
   * @param inputStream input image stream
   * @param factor      as double
   * @return Either an error message or the encoded bytes
   */
  def scale(inputStream: InputStream, factor: Double): TransformationBytesResult =
    withImageStream(inputStream)(_.scale(factor), "Error scaling stream")

  /**
   * Close up
   *
   * @param image  input
   * @param factor as double
   * @return Either an error message or the output File
   */
  def zoom( image: File, outputDir: File,factor: Double): TransformationResult =
    withImage(image, outputDir)(_.zoom(factor),"Error zooming in")

  /**
   * Close up (as bytes)
   *
   * @param inputBytes input image bytes
   * @param factor     as double
   * @return Either an error message or the encoded bytes
   */
  def zoom(inputBytes: Array[Byte], factor: Double): TransformationBytesResult =
    withImageBytes(inputBytes)(_.zoom(factor), "Error zooming in bytes")

  /**
   * Close up (as a stream)
   *
   * @param inputStream input image stream
   * @param factor      as double
   * @return Either an error message or the encoded bytes
   */
  def zoom(inputStream: InputStream, factor: Double): TransformationBytesResult =
    withImageStream(inputStream)(_.zoom(factor), "Error zooming in stream")

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
   * Takes a list of byte arrays and adds a border to them
   *
   * @param inputBytesSeq list of raw image byte arrays
   * @param color         RGBColor
   * @param thickness     factor as double
   * @return sequence of TransformationBytesResult
   */
  def addBorder(
                 inputBytesSeq: Seq[Array[Byte]],
                 color: RGBColor,
                 thickness: Int
               ): Seq[TransformationBytesResult] =
    inputBytesSeq.map(addBorder(_, color, thickness))

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
   * Takes a list of byte arrays and scales them
   *
   * @param inputBytesSeq list of raw image byte arrays
   * @param factor        factor as double
   * @return sequence of TransformationBytesResult
   */
  def scale(
             inputBytesSeq: Seq[Array[Byte]],
             factor: Double
           ): Seq[TransformationBytesResult] =
    inputBytesSeq.map(scale(_, factor))

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

  /**
   * Takes a list of byte arrays and zooms in them
   *
   * @param inputBytesSeq list of raw image byte arrays
   * @param factor        factor as double
   * @return sequence of TransformationBytesResult
   */
  def zoom(
            inputBytesSeq: Seq[Array[Byte]],
            factor: Double
          ): Seq[TransformationBytesResult] =
    inputBytesSeq.map(zoom(_, factor))

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

  /** Bytes counterpart of [[withImage]]: loads from bytes, transforms, and re-encodes to bytes
   * instead of writing to a File. Intended for API call sites that already have image bytes
   * in memory (e.g. from a multipart upload) and want bytes back rather than a file on disk.
   */
  private def withImageBytes(
                              input: Array[Byte],
                              writer: ImageWriter = WebpWriter.DEFAULT
                            )(
                              transform: ImmutableImage => ImmutableImage,
                              errorCtx: String
                            ): TransformationBytesResult =
    Try {
      val img = ImmutableImage.loader().fromBytes(input)
      val result = transform(img)
      result.bytes(writer)
    }.toEither.left.map(ex =>
      TransformationError(s"$errorCtx: ${ex.getMessage}")
    )

  /** Stream counterpart of [[withImage]]: loads from an InputStream, transforms, and re-encodes
   * to bytes. Intended for API call sites that receive the image as a stream (e.g. directly off
   * a request body) and want to avoid buffering it into a byte array themselves first.
   */
  private def withImageStream(
                               input: InputStream,
                               writer: ImageWriter = WebpWriter.DEFAULT
                             )(
                               transform: ImmutableImage => ImmutableImage,
                               errorCtx: String
                             ): TransformationBytesResult =
    Try {
      val img = ImmutableImage.loader().fromStream(input)
      val result = transform(img)
      result.bytes(writer)
    }.toEither.left.map(ex =>
      TransformationError(s"$errorCtx: ${ex.getMessage}")
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

  /** Bytes counterpart of [[writeAs]]: converts image bytes to the target format and returns
   * the encoded bytes instead of writing to a named output file.
   */
  private def writeAsBytes(
                            input: Array[Byte],
                            format: ImageFormat
                          ): TransformationBytesResult =
    Try {
      val image = ImmutableImage.loader().fromBytes(input)
      image.bytes(format.writer)
    }.toEither.left.map(ex =>
      TransformationError(s"Error converting bytes: ${ex.getMessage}")
    )

  /** Stream counterpart of [[writeAs]]: converts an image stream to the target format and
   * returns the encoded bytes.
   */
  private def writeAsStream(
                             input: InputStream,
                             format: ImageFormat
                           ): TransformationBytesResult =
    Try {
      val image = ImmutableImage.loader().fromStream(input)
      image.bytes(format.writer)
    }.toEither.left.map(ex =>
      TransformationError(s"Error converting stream: ${ex.getMessage}")
    )

  private def toggleDesktopAndMobile(thumb : ThumbType, image : ImmutableImage) : ImmutableImage = {
    thumb.deviceName match {
      case "desktop" => image.scaleTo(300, 300)
      case "mobile" => image.scaleTo(150, 150)
    }
  }

}