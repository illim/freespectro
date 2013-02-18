package priv.sp.gui

import priv._
import org.lwjgl.opengl.GL11._
import priv.sp._
import priv.World
import priv.GuiElem

class SlotButton(val num: Int, slot: => Option[SlotState], sp: SpWorld) extends GuiElem {
  import sp.baseTextures.slotTex
  val size = Coord2i(slotTex.width, slotTex.height)
  enabled = false
  private var card = getCard
  private var getDelta = Function.const[Long, Long](0) _

  def refresh() { card = getCard }
  def isEmpty = card.isEmpty

  private def getCard = slot.map { c => (c, sp.textures.get("Images/Cards/" + c.card.image)) }

  val slotSize = Coord2i(120, 142)

  def render(world: World) {
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
    glColor4f(1, 1, 1, 1)

    if (enabled) {
      tex.draw(sp.baseTextures.cardGlow)
    }

    tex.draw(slotTex.id, slotSize)

    card.foreach {
      case (cardState, cardTex) =>
        glPushMatrix()
        glTranslatef(21, 33 + getDelta(world.time), 0)
        tex.draw(cardTex)
        glTranslatef(-3, -8, 0)
        tex.draw(sp.baseTextures.borderTex)
        Fonts.draw(72, 1, cardState.card.cost, 'blue)
        Fonts.draw(4, 80, cardState.attack, 'red)
        Fonts.draw(70, 80, cardState.life, 'green)
        glPopMatrix()
    }
  }

  case class AnimTask[A](direction: Int)(onEnd: => A) extends Task[A] {
    val duration = 1500L
    private val half = duration / 2
    private val amplitude = 2
    def init() { getDelta = delta _ }
    def end() = {
      getDelta = Function.const[Long, Long](0) _
      onEnd
    }
    private def delta(time: Long) = amplitude * direction * (half - math.abs(half - (time - start))) / 100
  }
}
