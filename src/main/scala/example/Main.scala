package example

import coloring.RGBColor
import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.webp.WebpWriter
import web.guidelines.{BackgroundImage, HeroImage, WebsiteImageType}
import web.utils.Utils
import java.awt.Color
import java.io.{File, IOException}

object Main extends App {
  //folders setup
  val inputDir = new File("input")
  val outputDir = new File("output")
  outputDir.mkdirs()

  //listing images
  val images = Utils.listImages(inputDir)
  println(s" ${images.size} found ${inputDir.getPath}:")
  images.foreach(f => println(s"  - ${f.getName}"))

  //converting to webp
  val webpResults = Utils.convertToWebp(images, outputDir)
  webpResults.foreach {
    case Right(f) => println(s"WebP at: ${f.getName}")
    case Left(err) => println(s"Error: $err")
  }

  //making thumbnails
  val thumbsDesktop = Utils.createThumbnail(images, outputDir, "desktop")
  val thumbsMobile = Utils.createThumbnail(images, outputDir, "mobile")

  println("Thumbnails desktop:")
  thumbsDesktop.foreach {
    case Right(f) => println(s"  - ${f.getName}")
    case Left(err) => println(s"  - Error: $err")
  }

  println("Thumbnails mobile:")
  thumbsMobile.foreach {
    case Right(f) => println(s"  - ${f.getName}")
    case Left(err) => println(s"  - Error: $err")
  }

  //now placeholders
  val placeholders = Utils.generatePlaceholders(
    number = 3,
    width = 200,
    height = 200,
    fillColor = Some(Color.RED),
    applyBlur = true,
    outputDir = outputDir
  )

  placeholders.foreach {
    case Right(f) => println(s"placeholder generated: ${f.getName}")
    case Left(err) => println(s"Error: $err")
  }

  //test OCR preprocessing
  images.headOption.foreach { imgFile =>
    println("testing ocr processing...")

    val image = ImmutableImage.loader().fromFile(imgFile)
    val processed = Utils.OCR.prepareOCR(
      image,
      tilt = 5.0,
      contrastFactor = 1.3,
      threshold = 128,
      doBinarize = true
    )

    val outPath = new File(outputDir, "ocr_processed.webp").getPath
    processed.output(WebpWriter.MAX_LOSSLESS_COMPRESSION, new File(outPath))
    println(s"OCR processed stored at: $outPath")
  }

  println(" complete ")

  println("=== color tests ===")

  // create a color
  val redColor = RGBColor(100, 50, 200)
  println(s"initial color: $redColor, hex=${redColor.toHex}")

  // increase channels
  val brighter = redColor.increaseAll(50, 30, -100)
  println(s"adjusted color: $brighter, hex=${brighter.toHex}")

  // mix 'em up
  val blueColor = RGBColor(0, 0, 255)
  val mixed = redColor.mixWith(blueColor, 0.5)
  println(s"50% mix: $mixed, hex=${mixed.toHex}")

  // from hex
  val fromHex = RGBColor.fromHex("#ff00cc")
  println(s"from hex '#ff00cc': $fromHex")

  // random color
  val randomColor = RGBColor.random()
  println(s"random color: $randomColor, hex=${randomColor.toHex}")

  println("=== WebsiteImageType tests ===")

  // list available images and its guidelines
  println("Resumen de imágenes:")
  WebsiteImageType.summary()

  // testing specific types
  val bg = BackgroundImage()
  println(s"background image: ${bg.name}, ratio=${bg.ratio}, desktop=${bg.desktop}, mobile=${bg.mobile}")

  val hero = HeroImage()
  println(s"hero image: ${hero.name}, ratio=${hero.ratio}, desktop=${hero.desktop}, mobile=${hero.mobile}")

  // search by name
  WebsiteImageType.fromName("logo_square") match {
    case Some(img) => println(s"square logo found: ${img.desktop} x ${img.mobile}, ratio=${img.ratio}")
    case None => println("square logo not found")
  }


}
