package priv

import java.awt.Font
import org.newdawn.slick.{ UnicodeFont, Color }
import org.newdawn.slick.util.ResourceLoader
import org.newdawn.slick.font.effects._
import org.lwjgl.opengl.GL11
import org.lwjgl.opengl.GL11._

object Fonts {
  private val awtFont = new Font("Times New Roman", Font.BOLD, 14)
  private val awtFontBig = new Font("Times New Roman", Font.BOLD, 24)
  val font = new PimpFont(awtFont)
  val big = new PimpFont(awtFontBig)
}

class PimpFont(awtFont: Font) {
  val font = new UnicodeFont(awtFont)
  font.addAsciiGlyphs();
  font.getEffects().asInstanceOf[java.util.List[Effect]].add(new ColorEffect())
  font.loadGlyphs()

  val darkGreen = Color.green.darker()

  def draw(x: Int, y: Int, s: Any, color: Symbol = null) {
    GL11.glBlendFunc(GL11.GL_SRC_ALPHA, GL11.GL_ONE_MINUS_SRC_ALPHA)
    val c = if (color == null) Color.black else {
      color match {
        case 'white ⇒ Color.white
        case 'blue  ⇒ Color.blue
        case 'red   ⇒ Color.red
        case 'green ⇒ darkGreen
        case 'gray  ⇒ Color.gray
        case _      ⇒ Color.black
      }
    }
    font.drawString(x, y, s.toString, c)
    //    glColor4f(1, 1, 1, 1) // recover color
  }

  def getWidth(s: String) = font.getWidth(s)
  def getHeight(s: String) = font.getHeight(s)
}
