package priv.sp.gui

import priv._
import org.lwjgl.opengl.GL11._
import priv.sp._
import priv.World
import priv.GuiElem

// total crap
class SlotButton(val num: Int, playerId : PlayerId, slot: => Option[SlotState], game : Game) extends GuiElem with Damagable {
  import game.sp.baseTextures.slotTex

  val direction = if (playerId == owner) -1 else 1
  val size = slotTex.size
  enabled = false
  private var card = getCard
  private var runAnim = Option.empty[RunAnimTask[_]]
  private var focusAnim = Option.empty[FocusAnimTask[_]]
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
      case (slotState, cardTex) =>
        glPushMatrix()
        glTranslatef(21, 33 + runAnim.map(_.getDelta(world.time).floatValue).getOrElse(0f), 0)
        focusAnim.foreach{ anim =>
          val scale = 1 + anim.getDelta(world.time).toFloat
          glScalef(scale, scale, 1)
          val pos = Coord2i.recenter(cardTex.size * 0.5, cardTex.size * scale)
          glTranslatef(pos.x, pos.y, 0)
        }
        tex.draw(cardTex)
        glTranslatef(-3, -8, 0)
        tex.draw(game.sp.baseTextures.borderTex)
        Fonts.draw(72, 1, slotState.card.cost, 'blue)
        Fonts.draw(4, 80, slotState.attack, 'red)
        Fonts.draw(70, 80, slotState.life, 'green)
        getDamageAnimOpt.foreach{ anim =>
          Fonts.draw(70, 65 - anim.delta(world.time), anim.text, anim.color)
        }
        lifeBar(slotState)
        glPopMatrix()
    }

    if (enabled) {
      dash(dashOffset, 81, 92, ((deltaT(world.time) / 100) % 16).intValue)
    }
  }

  def lifeBar(slotState : SlotState) = {
    glDisable(GL_TEXTURE_2D)
    val w = 66 * slotState.life / slotState.card.life
    val h = 7
    glColor4f(0.2f, 0.6f, 0.2f, 0.6f)
    glBegin(GL_POLYGON)
    glVertex2f(0, 0)
    glVertex2f(w, 0)
    glVertex2f(w, h)
    glVertex2f(0, h)
    glEnd()
    glColor4f(1, 1, 1, 1f)
    glEnable(GL_TEXTURE_2D)
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

  class RunAnimTask[A](onEnd: => A) extends Task[A] {
    val duration = 1500L
    private val half = duration / 2
    private val amplitude = 2
    def init() { runAnim = Some(this) }
    def end() = {
      runAnim = None
      onEnd
    }
    def getDelta(time: Long) = amplitude * direction * (half - math.abs(half - (time - start))) / 100
  }

  class FocusAnimTask[A](onEnd: => A) extends Task[A] {
    val duration = 500L
    private val half = duration / 2
    private val amplitude = 0.05
    def init() { focusAnim = Some(this) }
    def end() = {
      focusAnim = None
      onEnd
    }
    def getDelta(time: Long) = amplitude * math.sin((time - start).toDouble / duration * math.Pi)
  }
}
