package priv.sp.gui

import priv._
import org.lwjgl.opengl.GL11._
import priv.sp._
import priv.World
import priv.GuiElem

class SlotButton(val num: Int, playerId : PlayerId, slot: => Option[SlotState], game : Game) extends GuiElem {
  import game.sp.baseTextures.slotTex

  val direction = if (playerId == owner) -1 else 1
  val size = Coord2i(slotTex.width, slotTex.height)
  enabled = false
  private var card = getCard
  private var getDelta = zeroAnim
  private var getDamageAnimOpt = Option.empty[DamageAnimTask]

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

  val slotSize = Coord2i(120, 142)

  def render(world: World) {
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
    glColor4f(1, 1, 1, 1)

    if (enabled) {
      tex.draw(game.sp.baseTextures.cardGlow)
    }

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

  case class DamageAnimTask(damage : Int) extends Task[Unit] {
    val duration = 1000L
    val text = if (damage > 0) "+"+damage else damage
    val color = if (damage > 0) 'green else 'red
    private val amplitude = 2
    def init() { getDamageAnimOpt = Some(this) }
    def end() = { getDamageAnimOpt = None }
    def delta(time: Long) = (amplitude * (time - start) / 100).intValue
  }
}
