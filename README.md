# 🖼️ Scala Web Image Toolkit

> A lightweight, functional-oriented image processing toolkit written in **Scala**, designed for **web development** workflows.

This project demonstrates how to handle image transformations like conversion to WebP, thumbnail generation, color manipulation, and OCR preprocessing; using a **pure functional style** with the [Scrimage](https://github.com/sksamuel/scrimage) library.

---

## ✨ Features

- ✅ **Convert images to WebP** with optimal compression
- ✅ **Generate thumbnails** for desktop and mobile
- ✅ **Create blurred placeholders** for web performance
- ✅ **Perform OCR preprocessing** (binarization, tilt, contrast)
- ✅ **Manipulate and blend colors** functionally
- ✅ **Follow common web image type guidelines**

---

## 🧰 Tech Stack

| Component | Description |
|------------|-------------|
| **Scala** | Functional programming language |
| **Scrimage** | Image manipulation library |
| **Java AWT** | For color and basic graphics |
| **WebP Writer** | High-efficiency image output |

---

### image creation
````scala
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
````
 ### color examples
```scala
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
println(s"random color: $randomColor, hex=${randomColor.toHex}"
```
### output
```
initial color: RGBColor(100,50,200), hex=#6432C8
adjusted color: RGBColor(150,80,100), hex=#965064
50% mix: RGBColor(50,25,227), hex=#3219E3
from hex '#ff00cc': RGBColor(255,0,204)
random color: RGBColor(75,123,240), hex=#4B7BF0
```

### web guidelines examples
```scala
WebsiteImageType.summary()
WebsiteImageType.fromName("logo_square") match {
  case Some(img) => println(s"square logo found guidelines for desktop: ${img.desktop} " +
    s" for mobile: ${img.mobile}, ratio=${img.ratio}")
  case None => println("square logo not found")
```
### output
```
Type                 Desktop (WxH)        Mobile (WxH)         Ratio
----------------------------------------------------------------------
background           2560x1400            360x640              64:35
hero                 1280x720             360x200              16:9
banner               1200x400             360x120              3:1
blog                 1200x800             360x240              3:2
logo_rectangle       400x100              160x40               4:1
logo_square          100x100              60x60                1:1
favicon              16x16                16x16                1:1
social_icon          32x32                48x48                1:1
lightbox             1920x1080            360x640              16:9
thumbnail            300x300              90x90                1:1
product_thumbnail    300x300              150x150              1:1

square logo found guidelines for desktop: 100x100  for mobile: 60x60, ratio=1:1
```