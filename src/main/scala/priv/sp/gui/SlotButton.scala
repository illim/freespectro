package priv.sp.gui

import priv._
import org.lwjgl.opengl.GL11._
import priv.sp._
import priv.World
import priv.GuiElem

class SlotButton(val num: Int, playerId : PlayerId, slot: => Option[SlotState], game : Game) extends GuiElem with Damagable {
  import game.sp.baseTextures.slotTex

  val direction = if (playerId == owner) -1 else 1
  val size = slotTex.size
  enabled = false
  private var card = getCard
  private var getDelta = zeroAnim
  val slotSize = Coord2i(120, 142)
  private val dashOffset = Coord2i(slotSize.x/2 - 39, slotSize.y/2-44)

  def zeroAnim = Function.const[Long, Long](0) _
  def refresh() {
    val old = card
    card = getCard
    for(before <- old; after <- card ; val d = after._1.life - before._1.life if d != 0){
      game.world.addTask(DamageAnimTask(d))
    }
  }
  def isEmpty = card.isEmpty

  private def getCard = slot.map { c => (c, game.sp.textures.get("Images/Cards/" + c.card.image)) }


  def render(world: World) {
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
    glColor4f(1, 1, 1, 1)

    tex.draw(slotTex.id, slotSize)

    card.foreach {
      case (cardState, cardTex) =>
        glPushMatrix()
        glTranslatef(21, 33 + getDelta(world.time), 0)
        tex.draw(cardTex)
        glTranslatef(-3, -8, 0)
        tex.draw(game.sp.baseTextures.borderTex)
        Fonts.draw(72, 1, cardState.card.cost, 'blue)
        Fonts.draw(4, 80, cardState.attack, 'red)
        Fonts.draw(70, 80, cardState.life, 'green)
        getDamageAnimOpt.foreach{ anim =>
          Fonts.draw(70, 65 - anim.delta(world.time), anim.text, anim.color)
        }
        glPopMatrix()
    }

    if (enabled) {
      dash(dashOffset, 81, 92, ((deltaT(world.time) / 100) % 16).intValue)
    }
  }

  def dash(c : Coord2i, w:Int, h : Int, t : Int){
    glDisable(GL_TEXTURE_2D)
    glPolygonMode(GL_FRONT_AND_BACK, GL_LINE)
    val mask = 0xFF << t
    glLineStipple(1, (mask ^ (mask >> 16)).shortValue)
    glEnable(GL_LINE_STIPPLE)
    glBegin(GL_POLYGON)
      glVertex2f(c.x,c.y)
      glVertex2f(c.x + w,c.y)
      glVertex2f(c.x + w,c.y +h)
      glVertex2f(c.x,c.y+h)
    glEnd()
    glPolygonMode(GL_FRONT_AND_BACK, GL_FILL)
    glDisable(GL_LINE_STIPPLE)
    glEnable(GL_TEXTURE_2D)
  }

  class AnimTask[A](onEnd: => A) extends Task[A] {
    val duration = 1500L
    private val half = duration / 2
    private val amplitude = 2
    def init() { getDelta = delta _ }
    def end() = {
      getDelta = zeroAnim
      onEnd
    }
    private def delta(time: Long) = amplitude * direction * (half - math.abs(half - (time - start))) / 100
  }
}
