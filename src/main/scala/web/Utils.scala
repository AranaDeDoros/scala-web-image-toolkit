package web

import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.webp.WebpWriter
import java.io.{File, IOException}

case class ImageConverter(
                           inputFolder: String,
                           outputFolder: String,
                           imageFiles: Seq[File] = Seq.empty
                         ) {

  private val supportedExts = Set("png", "jpg", "jpeg", "gif", "bmp")
  private val inputDir = new File(inputFolder)
  private val outputDir = new File(outputFolder)

  def build(): Either[String, ImageConverter] = {
    if (inputFolder.isEmpty || outputFolder.isEmpty)
      return Left("usage: ImageConverter(<input_folder>, <output_folder>)")

    if (!inputDir.exists() || !inputDir.isDirectory)
      return Left(s"no valid input folder: ${inputDir.getAbsolutePath}")

    if (!outputDir.exists()) outputDir.mkdirs()

    val files = Option(inputDir.listFiles()).getOrElse(Array.empty).filter { f =>
      val name = f.getName.toLowerCase
      supportedExts.exists(ext => name.endsWith("." + ext))
    }

    if (files.isEmpty)
      Left("no valid images found.")
    else {
      //returns a shallow copy and updates the seq
      Right(copy(imageFiles = files))
    }
  }

  def convertToWebp(): Unit = {
    if (imageFiles.isEmpty) {
      println("no images loaded, run build() first.")
      return
    }

    println(s"converting ${imageFiles.length} images to WEBP...")
    imageFiles.foreach { file =>
      try {
        val image = ImmutableImage.loader().fromFile(file)
        val nameWithoutExt = file.getName.replaceAll("\\.[^.]+$", "")
        val outputFile = new File(outputDir, s"$nameWithoutExt.webp")
        image.output(WebpWriter.MAX_LOSSLESS_COMPRESSION, outputFile)
        println(s"${file.getName} → ${outputFile.getName}")
      } catch {
        case ex: IOException =>
          println(s"error converting ${file.getName}: ${ex.getMessage}")
      }
    }
    println("conversion complete")
  }

  def createThumbnail(prefix: String = "thumb_", thumbType: String = "desktop"): Unit = {
    if (imageFiles.isEmpty) {
      println("no images loaded, run build() first.")
      return
    }

    println(s"generating thumbnails ($thumbType) for ${imageFiles.length} images...")
    imageFiles.foreach { file =>
      try {
        val image = ImmutableImage.loader().fromFile(file)
        val outputFile = new File(outputDir, prefix + file.getName)
        val guideline = WebsiteImageTypes.get("ThumbnailImage")

        val dimension = thumbType.toLowerCase match {
          case "desktop" => guideline.flatMap(_.desktop)
          case _         => guideline.flatMap(_.mobile)
        }

        dimension match {
          case Some(dim) =>
            val scaled = image.scaleTo(dim.width, dim.height)
            scaled.output(WebpWriter.DEFAULT, outputFile)
            println(s"thumbnail (${dim.width}x${dim.height}) → ${outputFile.getName}")
          case None =>
            println(s"no dimensions set for '$thumbType'.")
        }
      } catch {
        case ex: IOException =>
          println(s"error making thumbnail ${file.getName}: ${ex.getMessage}")
      }
    }
    println("thumbnails generated")
  }
}
