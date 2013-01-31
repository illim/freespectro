package priv.sp.gui

import priv._
import priv.entity._
import org.lwjgl.opengl.GL11._
import priv.sp.SpWorld

case class SlotButton(num : Int, sp: SpWorld) extends GuiElem {
  import sp.baseTextures.slotTex
  val size = Coord(slotTex.width, slotTex.height)
  var enabled = false

  def render(world: World) {
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
    glColor4f(1, 1, 1, 1)
    
    if (enabled) {
      glPushMatrix()
      drawTexture(sp.baseTextures.cardGlow)
      glPopMatrix()
    }
    glPushMatrix()
    drawTexture(slotTex)
    glPopMatrix()
  }

}