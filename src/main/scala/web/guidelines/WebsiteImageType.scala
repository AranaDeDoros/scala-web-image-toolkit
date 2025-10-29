package web.guidelines
import scala.annotation.tailrec

case class Dimension(width: Int, height: Int)
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

trait WebsiteImageType {
  val dims: Dimension
  val ratio: AspectRatio
}

case class BackgroundImage(dim: Dimension, aspectRatio: AspectRatio)
  extends WebsiteImageType {
  override val dims: Dimension = dim
  override val ratio: AspectRatio = aspectRatio
}

object Test extends App {

  val tdim = Dimension(1920, 1080)
  val vi = BackgroundImage(tdim, AspectRatio(tdim))
  println(vi.aspectRatio)
}