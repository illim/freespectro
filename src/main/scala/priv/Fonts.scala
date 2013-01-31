package priv

import java.awt.Font
import org.newdawn.slick.{ UnicodeFont, Color }
import org.newdawn.slick.util.ResourceLoader
import org.newdawn.slick.font.effects.ColorEffect
import org.newdawn.slick.font.effects.Effect
import org.lwjgl.opengl.GL11
import org.lwjgl.opengl.GL11._

object Fonts {
  private val awtFont = new Font("Times New Roman", Font.BOLD, 14)
  val font = new UnicodeFont(awtFont)
  font.addAsciiGlyphs();
  font.getEffects().asInstanceOf[java.util.List[Effect]].add(new ColorEffect())
  font.loadGlyphs()

  def draw(x: Int, y: Int, s: String, color: Symbol = null) {
    GL11.glBlendFunc(GL11.GL_SRC_ALPHA, GL11.GL_ONE_MINUS_SRC_ALPHA)
    val c = if (color == null) Color.black else {
      color match {
        case 'white => Color.white
        case 'blue => Color.blue
        case 'red => Color.red
        case _ => Color.black
      }
    }
    Fonts.font.drawString(x, y, s, c)
    glColor4f(1, 1, 1, 1) // recover color
  }
}