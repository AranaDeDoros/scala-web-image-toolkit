package web.guidelines

import scala.annotation.tailrec
import scala.reflect.runtime.{universe => ru}
import ru._

case class Dimension(width: Int, height: Int) {
  override def toString: String = {
    s"${width}x${height}"
  }
}

case class AspectRatio(dim: Dimension) {
  private val width = this.dim.width
  private val height = this.dim.height

  def ratio = width / height

  @tailrec
  private def gcd(a: Int, b: Int): Int = {
    if (b == 0) {
      a
    }
    else {
      gcd(b, a % b)
    }
  }

  override def toString: String = {
    val gcd = this.gcd(this.width, this.height)
    s"${this.width / gcd}:${this.height / gcd}"
  }
}

sealed trait WebsiteImageType {
  val name: String
  val desktop: Dimension
  val mobile: Dimension
  val ratio: AspectRatio
}

// Website Image Types
case class BackgroundImage(
                            desktop: Dimension = Dimension(2560, 1400),
                            mobile: Dimension = Dimension(360, 640)
                          ) extends WebsiteImageType {
  override val name = "background"
  override val ratio = AspectRatio(desktop)
}

case class HeroImage(
                      desktop: Dimension = Dimension(1280, 720),
                      mobile: Dimension = Dimension(360, 200)
                    ) extends WebsiteImageType {
  override val name = "hero"
  override val ratio = AspectRatio(desktop)
}

case class WebsiteBanner(
                          desktop: Dimension = Dimension(1200, 400),
                          mobile: Dimension = Dimension(360, 120)
                        ) extends WebsiteImageType {
  override val name = "banner"
  override val ratio = AspectRatio(desktop)
}

case class BlogImage(
                      desktop: Dimension = Dimension(1200, 800),
                      mobile: Dimension = Dimension(360, 240)
                    ) extends WebsiteImageType {
  override val name = "blog"
  override val ratio = AspectRatio(desktop)
}

case class LogoRectangle(
                          desktop: Dimension = Dimension(400, 100),
                          mobile: Dimension = Dimension(160, 40)
                        ) extends WebsiteImageType {
  override val name = "logo_rectangle"
  override val ratio = AspectRatio(desktop)
}

case class LogoSquare(
                       desktop: Dimension = Dimension(100, 100),
                       mobile: Dimension = Dimension(60, 60)
                     ) extends WebsiteImageType {
  override val name = "logo_square"
  override val ratio = AspectRatio(desktop)
}

case class Favicon(
                    desktop: Dimension = Dimension(16, 16),
                    mobile: Dimension = Dimension(16, 16)
                  ) extends WebsiteImageType {
  override val name = "favicon"
  override val ratio = AspectRatio(desktop)
}

case class SocialMediaIcon(
                            desktop: Dimension = Dimension(32, 32),
                            mobile: Dimension = Dimension(48, 48)
                          ) extends WebsiteImageType {
  override val name = "social_icon"
  override val ratio = AspectRatio(desktop)
}

case class LightboxImage(
                          desktop: Dimension = Dimension(1920, 1080),
                          mobile: Dimension = Dimension(360, 640)
                        ) extends WebsiteImageType {
  override val name = "lightbox"
  override val ratio = AspectRatio(desktop)
}

case class ThumbnailImage(
                           desktop: Dimension = Dimension(300, 300),
                           mobile: Dimension = Dimension(90, 90)
                         ) extends WebsiteImageType {
  override val name = "thumbnail"
  override val ratio = AspectRatio(desktop)
}

case class ProductThumbnail(
                             desktop: Dimension = Dimension(300, 300),
                             mobile: Dimension = Dimension(150, 150)
                           ) extends WebsiteImageType {
  override val name = "product_thumbnail"
  override val ratio = AspectRatio(desktop)
}

// ==== Factory & Enum-like Registry ====

object WebsiteImageType {

  /** Lista con todos los tipos de imagen disponibles */
  lazy val all: Seq[WebsiteImageType] =  Seq(
    BackgroundImage(),
    HeroImage(),
    WebsiteBanner(),
    BlogImage(),
    LogoRectangle(),
    LogoSquare(),
    Favicon(),
    SocialMediaIcon(),
    LightboxImage(),
    ThumbnailImage(),
    ProductThumbnail())

  /** Búsqueda por nombre (case-insensitive) */
  def fromName(name: String): Option[WebsiteImageType] =
    all.find(_.name.equalsIgnoreCase(name))

  /** Muestra tabla resumen */
  def summary(): Unit = {
    println(f"${"Type"}%-20s ${"Desktop (WxH)"}%-20s ${"Mobile (WxH)"}%-20s ${"Ratio"}")
    println("-" * 70)
    all.foreach { img =>
      val d = img.desktop
      val m = img.mobile
      println(f"${img.name}%-20s ${s"${d.width}x${d.height}"}%-20s ${s"${m.width}x${m.height}"}%-20s ${img.ratio}")
    }
  }

  ///////////

}

object Test extends App {
  print(WebsiteImageType.all)
  WebsiteImageType.all.foreach { img =>
    println(f"${img.name}%-20s ${img.desktop.width}x${img.desktop.height}  ratio=${img.ratio}")
  }
  //  val bi = BackgroundImage()
  //  println(s"ratio: ${bi.ratio} " +
  //    s"desktop: ${bi.desktop} " +
  //    s"mobile: ${bi.mobile}")
  //
}




