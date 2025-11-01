package coloring

import java.awt.Color
import scala.util.Try

trait DigitalColor[C <: Channel, V, Self <: DigitalColor[C, V, Self]] {
  /** Generic way to modify a specific channel.
   * Takes a channel first, then a transformation function for its current value.
   */
  def modifyChannel(channel: C)(f: V => V): Self

  /** Increase a channel by an amount */
  def increaseChannel(channel: C, delta: V)(implicit numeric: Numeric[V]): Self =
    modifyChannel(channel)(v => numeric.plus(v, delta))

  /** Ensures the input of valid values for color modes */
  protected def clamp(value: V): V
}

/** Represents the available RGB color channels.
 * Each channel corresponds to one
 * component of the RGB color model.
 */

sealed trait Channel
sealed trait RGBChannel extends Channel

/** Represents the red channel of an RGB color. */
case object Red extends RGBChannel


/** Represents the green channel of an RGB color. */
case object Green extends RGBChannel

/** Represents the blue channel of an RGB color. */
case object Blue extends RGBChannel


/** Represents an RGB color with red, green, and blue components.
 *
 * @param red
 * the intensity of the red channel (0–255)
 * @param green
 * the intensity of the green channel (0–255)
 * @param blue
 * the intensity of the blue channel (0–255)
 *
 */

case class RGBColor(red: Int, green: Int, blue: Int)
  extends DigitalColor[RGBChannel, Int, RGBColor] {
  /** Converts this RGBColor instance into a java.awt.Color object.
   * @return
   * a Color object representing this RGB color
   */
  def color: Color = new Color(red, green, blue)

  /** Clamps a color channel value to the valid range [0, 255]. Values below 0
   * are set to 0, and values above 255 are set to 255.
   */
  override protected def clamp(value: Int): Int =
    if (value < 0) 0 else if (value > 255) 255 else value

  /** Increases or decreases a specific color channel (Red, Green, or Blue) by
   * the given amount, ensuring the resulting value stays within [0, 255].
   * @param channel
   * the color channel to modify
   * @param f
   * transformation function for the amount to add (can be negative)
   * @return
   * a new RGBColor with the modified channel
   */
  override def modifyChannel(channel: RGBChannel)(f: Int => Int): RGBColor =
    channel match {
    case Red   => copy(red = clamp(f(red)))
    case Green => copy(green = clamp(f(green)))
    case Blue  => copy(blue = clamp(f(blue)))
  }

  /** Increases or decreases all color channels simultaneously by different
   *
   * amounts. Each channel is clamped to stay within [0, 255].
   * @param deltaR
   * the amount to add to the red channel
   * @param deltaG
   * the amount to add to the green channel
   * @param deltaB
   * the amount to add to the blue channel
   * @return
   * a new RGBColor with adjusted channels
   */
  def increaseAll(deltaR: Int, deltaG: Int, deltaB: Int): RGBColor =
    RGBColor(
      clamp(red + deltaR),
      clamp(green + deltaG),
      clamp(blue + deltaB)
    )

  /** Blends this color with another color based on the given ratio. A ratio of
   * 0.0 returns this color; a ratio of 1.0 returns the other color.
   * @param other
   * the other color to mix with
   * @param ratio
   * the blend ratio between 0.0 and 1.0
   * @return
   * a new RGBColor representing the blended color
   * @throws IllegalArgumentException
   * if the ratio is outside [0.0, 1.0]
   */
  def mixWith(other: RGBColor, ratio: Double): RGBColor = {
    require(ratio >= 0.0 && ratio <= 1.0, "ratio must be between 0.0 and 1.0")
    def lerp(a: Int, b: Int): Int = (a + (b - a) * ratio).toInt
    RGBColor(
      clamp(lerp(red, other.red)),
      clamp(lerp(green, other.green)),
      clamp(lerp(blue, other.blue))
    )
  }

  /** Returns the hexadecimal string representation of this color, typically
   * used in web development (e.g., "#ff00cc").
   * @return
   * a lowercase hexadecimal color string
   */
  def toHex: String = f"#$red%02x$green%02x$blue%02x"
}

/** Companion object for [[RGBColor]] providing utility factory methods.
 */

object RGBColor {
  /** Creates an RGBColor from a hexadecimal color string. The input may
   * optionally start with a "#" (e.g., "#ff00cc" or "ff00cc").
   * @param hex
   * the hexadecimal color string (3, 6, or 8 hex digits)
   * @return
   * an Option containing the parsed RGBColor, or None if parsing fails
   */

  def fromHex(hex: String): Option[RGBColor] = {
    val cleanHex = hex.replace("#", "").trim.toLowerCase
    Try {
      cleanHex.length match {
        case 3 =>
          val r = Integer.parseInt(cleanHex.charAt(0).toString * 2, 16)
          val g = Integer.parseInt(cleanHex.charAt(1).toString * 2, 16)
          val b = Integer.parseInt(cleanHex.charAt(2).toString * 2, 16)
          RGBColor(r, g, b)
        case 6 =>
          val r = Integer.parseInt(cleanHex.substring(0, 2), 16)
          val g = Integer.parseInt(cleanHex.substring(2, 4), 16)
          val b = Integer.parseInt(cleanHex.substring(4, 6), 16)
          RGBColor(r, g, b)
        case 8 =>
          // Ignore alpha channel if present
          val r = Integer.parseInt(cleanHex.substring(2, 4), 16)
          val g = Integer.parseInt(cleanHex.substring(4, 6), 16)
          val b = Integer.parseInt(cleanHex.substring(6, 8), 16)
          RGBColor(r, g, b)
      }
    }.toOption
  }

  /** Creates an RGBColor instance from a java.awt.Color object.
   * @param color
   * the java.awt.Color instance
   * @return
   * a corresponding RGBColor
   */

  def fromColor(color: Color): RGBColor =
    RGBColor(color.getRed, color.getGreen, color.getBlue)

  /** Generates a random RGBColor.
   * @return
   * a new RGBColor with random red, green, and blue values
   */

  def random(): RGBColor = {
    val rnd = new scala.util.Random
    RGBColor(rnd.nextInt(256), rnd.nextInt(256), rnd.nextInt(256))
  }
}

sealed trait CMYKChannel extends Channel
case object Cyan extends CMYKChannel
case object Magenta extends CMYKChannel
case object Yellow extends CMYKChannel
case object Key extends CMYKChannel

case class CMYKColor(cyan: Int, magenta: Int, yellow: Int, key: Int)
  extends DigitalColor[CMYKChannel, Int, CMYKColor] {

  /**Ensures channel values stay within 0–100 */
  override protected def clamp(x: Int): Int =
    if (x < 0) 0 else if (x > 100) 100 else x

  /**Curried higher-order modifier for channel transformations */
  override def modifyChannel(channel: CMYKChannel)(f: Int => Int): CMYKColor =
    channel match {
    case Cyan    => copy(cyan = clamp(f(cyan)))
    case Magenta => copy(magenta = clamp(f(magenta)))
    case Yellow  => copy(yellow = clamp(f(yellow)))
    case Key     => copy(key = clamp(f(key)))
  }

  /**lighten/darken overall color */
  def adjustAll(f: Int => Int): CMYKColor =
    CMYKColor(
      clamp(f(cyan)),
      clamp(f(magenta)),
      clamp(f(yellow)),
      clamp(f(key))
    )

  /**human-readable debug string */
  override def toString: String =  s"CMYK(c=$cyan%, m=$magenta%, y=$yellow%, k=$key%)"
}