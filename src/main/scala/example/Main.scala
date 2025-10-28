package example

import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.webp.WebpWriter
import web.ImageConverter

import java.io.{File, IOException}

object ImageUtils extends  App{

  val ic =  ImageConverter("input", "output")
  ic.build() match {
    case Right(ready) =>
      ready.convertToWebp()
      ready.createThumbnail("thumb_", "mobile")

    case Left(error) =>
      println(s"Error: $error")
  }
}
