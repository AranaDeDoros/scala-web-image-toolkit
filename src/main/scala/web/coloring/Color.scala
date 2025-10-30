package coloring

import java.awt.Color
import scala.util.Try

/** Represents the available RGB color channels.
 * Each channel corresponds to one
 * component of the RGB color model.
 */

sealed trait Channel

/** Represents the red channel of an RGB color. */
case object Red extends Channel

/** Represents the green channel of an RGB color. */
case object Green extends Channel

/** Represents the blue channel of an RGB color. */
case object Blue extends Channel


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

case class RGBColor(red: Int, green: Int, blue: Int) {
  /** Converts this RGBColor instance into a java.awt.Color object.
   * @return
   * a Color object representing this RGB color
   */
  def color: Color = new Color(red, green, blue)

  /** Clamps a color channel value to the valid range [0, 255]. Values below 0
   * are set to 0, and values above 255 are set to 255.
   */
  private val clamp: PartialFunction[Int, Int] = {
    case x if x < 0 => 0
    case x if x > 255 => 255
    case x => x
  }

  /** Increases or decreases a specific color channel (Red, Green, or Blue) by
   * the given amount, ensuring the resulting value stays within [0, 255].
   * @param channel
   * the color channel to modify
   * @param amount
   * the amount to add (can be negative)
   * @return
   * a new RGBColor with the modified channel
   */

  def increaseChannel(channel: Channel, amount: Int): RGBColor = channel match {
    case Red => copy(red = clamp(red + amount))
    case Green => copy(green = clamp(green + amount))
    case Blue => copy(blue = clamp(blue + amount))
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

 