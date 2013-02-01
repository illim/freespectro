package priv.sp.gui

import priv._
import priv.entity._
import org.lwjgl.opengl.GL11._
import priv.sp._

case class SlotButton(num: Int, slotRef : StateRef[Option[CardState]], sp: SpWorld) extends GuiElem {
  import sp.baseTextures.slotTex
  val size = Coord2i(slotTex.width, slotTex.height)
  enabled = false
  private var card = getCard

  def refresh() { card = getCard }
  def isEmpty = card.isEmpty
  
  private def getCard = slotRef.get.map{ c => (c, sp.textures.get("Images/Cards/" + c.card.image)) }

  val slotSize = Coord2i(120, 142)

  def render(world: World) {
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
    glColor4f(1, 1, 1, 1)

    if (enabled) {
      drawTexture(sp.baseTextures.cardGlow)
    }

    card.foreach {
      case (cardState, cardTex) =>
        glPushMatrix()
        glTranslatef(21, 33, 0)
        drawTexture(cardTex)
        glTranslatef(-3, -8, 0)
        drawTexture(sp.baseTextures.borderTex)
        Fonts.draw(72, 1, cardState.card.cost, 'blue)
        Fonts.draw(4, 80, cardState.attack, 'red)
        Fonts.draw(70, 80, cardState.life, 'green)
        glPopMatrix()
    }
    drawTexture(slotTex.id, slotSize)
  }

}