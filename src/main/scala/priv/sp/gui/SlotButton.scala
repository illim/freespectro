package priv.sp.gui

import priv._
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL20._
import priv.sp._
import priv.sp.house._
import priv.World
import priv.GuiElem

// total crap
class SlotButton(val num: Int, playerId: PlayerId, getInfo: ⇒ (Option[SlotState], Boolean), game: Game) extends GuiElem with Damagable {
  import game.sp.baseTextures._

  val direction = if (playerId == game.myPlayerId) -1 else 1
  val size = slotTex.size
  enabled = false
  private var content = toContent(getInfo)
  private var moveAnim = Option.empty[MoveAnimTask]
  private var alpha = 1f
  var focusScale = Option.empty[Float]
  val slotSize = Coord2i(120, 142)
  val stunPos = Coord2i.recenter(Coord2i(40, 40), stunTex.size)
  private val dashOffset = Coord2i(slotSize.x / 2 - 40, slotSize.y / 2 - 44)
  val location = Location(Coord2i(19, 33))
  private val fade = game.sp.baseShaders.fade
  private var decorate = Option.empty[() ⇒ Unit]

  def refresh() {
    val old = content
    content = toContent(getInfo)
    decorate = None
    alpha = 1f
    content._1 foreach { c ⇒
      val slotState = c._1
      val card = slotState.card
      if (slotState has CardSpec.pausedFlag) alpha = 0.5f
      if (slotState.status > 1) {
        decorate = Some(() ⇒ decorateStatus(slotState))
      } else if (card.isInstanceOf[MereMortal]) {
        decorate = Some(() ⇒ tex draw mortalTex)
      } else if (card.houseId == game.sp.houses.moutainKing.MoutainKing.houseId && slotState.data == Hird) {
        decorate = Some(() ⇒ tex draw hirdTex)
      }
      for {
        before ← old._1
        val d = slotState.life - before._1.life if d != 0
      } game.world addTask DamageAnimTask(d)
    }
  }
  def isEmpty = content._1.isEmpty

  private def toContent(info: (Option[SlotState], Boolean)) = (info._1.map { c ⇒ (c, game.sp.textures.get("Images/Cards/" + c.card.image)) }, info._2)

  def render() {
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)

    glColor4f(1, 1, 1, 1)
    if (content._2) {
      tex.draw(slotTex.id, slotSize)
    }

    content._1.foreach {
      case (slotState, cardTex) ⇒
        glPushMatrix()
        glTranslatef(
          location.c.x + moveAnim.map(_.getDelta(world.time).floatValue).getOrElse(0f),
          location.c.y,
          0)
        focusScale foreach { scale ⇒
          glScalef(scale, scale, 1)
          val pos = Coord2i.recenter(cardTex.size * 0.5, cardTex.size * scale)
          glTranslatef(pos.x, pos.y, 0)
        }
        if (alpha != 1) {
          fade.begin()
          glUniform1f(fade.fact, alpha)
        }
        tex draw cardTex
        decorate.foreach(_())
        glTranslatef(-3, -8, 0)
        tex draw game.sp.baseTextures.borderTex
        Fonts.font.draw(72, 1, slotState.card.cost, 'blue)
        Fonts.font.draw(4, 80, slotState.attack, 'red)
        Fonts.font.draw(70, 80, slotState.life, 'green)
        getDamageAnimOpt foreach { anim ⇒
          Fonts.font.draw(70, 65 - anim.delta(world.time), anim.text, anim.color)
        }
        lifeBar(slotState)
        if (alpha != 1) {
          fade.end()
        }
        glPopMatrix()
    }

    if (enabled) {
      dash(dashOffset, 81, 92, ((getDelta() / 100) % 16).intValue)
    }
  }

  def lifeBar(slotState: SlotState) = {
    glDisable(GL_TEXTURE_2D)
    val w = 66 * slotState.life / math.max(slotState.life, slotState.card.life)
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

  def dash(c: Coord2i, w: Int, h: Int, t: Int) {
    glDisable(GL_TEXTURE_2D)
    glPolygonMode(GL_FRONT_AND_BACK, GL_LINE)
    val mask = 0xFF << t
    glLineStipple(1, (mask ^ (mask >> 16)).shortValue)
    glEnable(GL_LINE_STIPPLE)
    glBegin(GL_POLYGON)
    glVertex2f(c.x, c.y)
    glVertex2f(c.x + w, c.y)
    glVertex2f(c.x + w, c.y + h)
    glVertex2f(c.x, c.y + h)
    glEnd()
    glPolygonMode(GL_FRONT_AND_BACK, GL_FILL)
    glDisable(GL_LINE_STIPPLE)
    glEnable(GL_TEXTURE_2D)
  }

  def decorateStatus(s: SlotState) {
    if (s has CardSpec.blockedFlag) {
      tex.drawAt(stunPos, crystalTex.id, crystalTex.size)
    } else if (s has CardSpec.stunFlag) {
      tex.drawAt(stunPos, stunTex.id, stunTex.size)
    } else if (s has CardSpec.invincibleFlag) {
      tex draw shieldTex
    } else if (s has CardSpec.cursedFlag) {
      tex draw deathTex
    }
  }

  def summon(start: Coord2i, s: SlotState) = {
    location.c = start - coord
    content = toContent((Some(s), true))
    new MoveTo(location, location.init)
  }

  class MoveTo(val location: Location, dest: Coord2i) extends TimedEntity {
    private val start = location.c
    private val dir = dest - start
    private val moveDuration = (dir.size * 0.7).toLong
    val duration = moveDuration + 100 // to fix imprecision at the end

    def render() {
      val fact = getDelta() / moveDuration.toFloat
      location.c = dest - (dir * math.max(0, (1 - fact)))
    }
    override def onEnd() {
      location.c = dest
    }
  }

  class Fade extends TimedEntity {
    val duration = 100L

    def render() {
      alpha = math.max(1 - getDelta() / duration.toFloat, 0)
    }
  }

  class Focus extends TimedEntity {
    val duration = 500L
    private val half = duration / 2
    private val amplitude = 0.05

    def render() {
      val delta = amplitude * math.sin(getDelta().toDouble / duration * math.Pi)
      focusScale = Some(1 + delta.toFloat)
    }
    override def onEnd() {
      focusScale = None
    }
  }

  class MoveAnimTask(dest: Int, lock: AnyRef) extends Task[Unit] {
    val duration = math.abs(dest - num) * 300L
    def init() { moveAnim = Some(this) }
    def end() = {
      lock.synchronized { lock.notifyAll() }
      moveAnim = None
      if (dest != num) {
        content = (None, true) // HACK
      }
    }
    def getDelta(time: Long) = (dest - num) * size.x * (time - start) / duration
  }

  on {
    case MouseMoved(_) ⇒
      game.descriptionPanel.describedOption = content._1.map(_._1.card)
    case MouseLeaved(_) ⇒
      game.descriptionPanel.describedOption = None
  }
}
