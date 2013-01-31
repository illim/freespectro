package priv.sp.gui

import priv._
import priv.entity._
import org.lwjgl.opengl.GL11._
import priv.sp.CardState
import priv.sp.Creature
import priv.sp.SpWorld
import priv.sp.Spell

case class CardButton(cardState: CardState, sp: SpWorld) extends GuiElem {
  private val cardTex = sp.textures.get("Images/Cards/" + cardState.card.image)
  private val (borderTex, maskTex) = sp.baseTextures.getBorder(cardState.card)
  val size = Coord(borderTex.width, borderTex.height)
  private var hovered = false
  var enabled = true

  def render(world: World) {
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
    glColor4f(1, 1, 1, 1)
    val isActive = cardState.isAvailable && enabled

    if (isActive && hovered) {
      glPushMatrix()
      glTranslatef(-5, -5, 0)
      drawTexture(sp.baseTextures.cardGlow)
      glPopMatrix()
    }

    glPushMatrix()
    if (cardState.card.isSpell) glTranslatef(-2, 1, 0) else glTranslatef(3, 8, 0)
    drawTexture(cardTex)
    glPopMatrix()

    drawTexture(borderTex)

    cardState.card match {
      case spell: Spell =>
        Fonts.draw(72, 9, spell.cost.toString, 'blue)
      case creature: Creature =>
        Fonts.draw(72, 1, creature.cost.toString, 'blue)
        Fonts.draw(4, 80, creature.attack.map(_.toString) getOrElse "?", 'red)
        Fonts.draw(70, 80, creature.life.toString, 'green)
    }

    if (!isActive) {
      glBlendFunc(GL_ONE_MINUS_DST_COLOR, GL_ONE)
      drawTexture(maskTex.id, borderTex.width, borderTex.height)
    }
  }

  on {
    case MouseMoved(_) => hovered = true
    case MouseLeaved(_) => hovered = false
  }

}