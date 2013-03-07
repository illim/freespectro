package priv.sp.gui

import priv._
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL20._
import priv.sp._
import priv.util._

class CardButton(val card : Card, houseState: => HouseState, sp: SpWorld) extends GuiElem {

  private val cardTex = sp.textures.get("Images/Cards/" + card.image)
  private val (borderTex, maskTex) = sp.baseTextures.getBorder(card)
  val size = Coord2i(borderTex.width, borderTex.height)
  private var hovered = false
  private val grey = sp.shaders.get("grey")
  private val hoverGlow = sp.baseShaders.hoverGlow
  private val selectedGlow = sp.baseShaders.selectedGlow("selcard", 200)
  var selected = false

  def render(world: World) {
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)

    val isActive = card.isAvailable(houseState) && enabled
    glColor4f(1, 1, 1, 1)

    if (!isActive) {
      grey.begin()
    } else if (isActive && hovered && !selected) {
      glPushMatrix()
      glTranslatef(-5, -5, 0)
      hoverGlow.used {
        val deltax = deltaT(world.time) / 100f
        val animLength = 50
        val animationCursor = deltax.intValue % animLength
        glUniform1i(hoverGlow.cursor, animationCursor)
        tex.draw(sp.baseTextures.cardGlow)
      }
      glPopMatrix()
    } else if (selected){
      val o = Coord2i(size.x/2 - 100, size.y/2 -100)
      glDisable(GL_TEXTURE_2D)
      selectedGlow.used {
        val deltax = deltaT(world.time) / 100f
        val animLength = 62
        val animationCursor = deltax % animLength
        glUniform1f(selectedGlow.cursor, animationCursor)
        glUniform2f(selectedGlow.offset, o.x, o.y)
        glBegin(GL_POLYGON)
        glVertex2f(o.x, o.y)
        glVertex2f(o.x + 200,o.y)
        glVertex2f(o.x + 200,o.y + 200)
        glVertex2f(o.x, o.y + 200)
        glEnd()
        glEnable(GL_TEXTURE_2D)
      }
    }

    glPushMatrix()
    if (card.isSpell) glTranslatef(-1, -1, 0) else glTranslatef(3, 8, 0)
    tex.draw(cardTex)
    glPopMatrix()

    tex.draw(borderTex)

    card match {
      case spell: Spell =>
        Fonts.draw(72, 9, spell.cost, 'blue)
      case creature: Creature =>
        Fonts.draw(72, 1, creature.cost, 'blue)
        Fonts.draw(4, 80, creature.attack.map(_.toString) getOrElse "?", 'red)
        Fonts.draw(70, 80, creature.life, 'green)
    }
    if (!isActive) grey.end()
  }

  on {
    case MouseMoved(_) => hovered = true
    case MouseLeaved(_) => hovered = false
  }

}

case class TestButton(sp: SpWorld) extends GuiElem {
  val size = Coord2i(200, 200)//sp.baseTextures.blank.coord
  val selectedGlow = sp.baseShaders.selectedGlow("test", size.x)
  def render(world: World) {
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
    glDisable(GL_TEXTURE_2D)
    glColor4f(1, 1, 1, 1)
    selectedGlow.used {
      val deltax = deltaT(world.time) / 50f
      val animLength = 50
      val animationCursor = deltax % animLength
      val o = Coord2i(0, 0)
      glUniform1f(selectedGlow.cursor, animationCursor)
      glUniform2f(selectedGlow.offset, 0, 0)
      glBegin(GL_POLYGON)
      glVertex2f(o.x, o.y)
      glVertex2f(o.x + size.x,o.y)
      glVertex2f(o.x + size.x,o.y + size.y)
      glVertex2f(o.x, o.y + size.y)
      glEnd()
      glEnable(GL_TEXTURE_2D)
    }
  }
  override def updateCoord(c : Coord2i){
    super.updateCoord(c)
    println("testcoord" + c)
  }
}
