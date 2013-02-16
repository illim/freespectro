package priv.sp.gui

import priv._
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL20._
import priv.sp._
import priv.util._

case class CardButton(cardState: HouseCardState, sp: SpWorld) extends GuiElem {

  private val cardTex = sp.textures.get("Images/Cards/" + cardState.card.image)
  private val (borderTex, maskTex) = sp.baseTextures.getBorder(cardState.card)
  val size = Coord2i(borderTex.width, borderTex.height)
  private var hovered = false
  private val grey = sp.shaders.get("grey")
  private val hoverGlow = sp.baseShaders.hoverGlow

  def render(world: World) {
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)

    val isActive = cardState.isAvailable && enabled
    glColor4f(1, 1, 1, 1)

    if (!isActive) {
      grey.begin()
    } else if (isActive && hovered) {
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
    }

    glPushMatrix()
    if (cardState.card.isSpell) glTranslatef(-2, 1, 0) else glTranslatef(3, 8, 0)
    tex.draw(cardTex)
    glPopMatrix()

    tex.draw(borderTex)

    cardState.card match {
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
  private val hoverGlow = sp.baseShaders.hoverGlow
  val size = sp.baseTextures.blank.coord
  def render(world: World) {
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)

    glColor4f(1, 1, 1, 1)
    hoverGlow.used {
      val deltax = deltaT(world.time) / 100f
      val animLength = 50
      val animationCursor = deltax.intValue % animLength
      println(animationCursor)
      glUniform1i(hoverGlow.cursor, animationCursor)
      tex.draw(sp.baseTextures.blank)
    }
  }

}