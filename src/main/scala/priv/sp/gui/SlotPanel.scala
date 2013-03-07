package priv.sp.gui

import priv._
import org.lwjgl.opengl.GL11._
import priv.sp._
import scala.util.continuations._

class SlotPanel(playerId : PlayerId, game : Game) {
  val lifeLabel = new LifeLabel(new DamagableInt(game.state.players(playerId).life, game), game)
  val slots =
    slotRange.map(num =>
      new SlotButton(
        num,
        playerId,
        game.state.players(playerId).slots.get(num),
        game)).toList
  val elts = lifeLabel :: /**testButton ::: */ slots

  def testButton = (if (playerId == owner) List(TestButton(game.sp)) else Nil)

  val slotOffset = lifeLabel.size
  val slotSize = slots(0).size
  slots.foreach(listenEvent _)

  val panel = Row(elts)

  abstract class SpellAnim(k : Int => Unit) extends Task[Unit] with Entity {
    def init() { panel.entities.add(this)  }
    def end() = {
      panel.unspawn(this)
      k(duration.intValue)
    }
  }

  // TODO
  class FlameAnimTask(cont : Int => Unit) extends SpellAnim(cont) {
    val duration = 1800L
    val fireTex = game.sp.baseTextures.fire

    def render(world: World) {
      glPushMatrix()
      glTranslatef(slotOffset.x + slotSize.x / 2,  slotSize.x / 2, 0)
      val delta = deltaT(world.time).intValue
      val animCursor = delta / 300
      slotRange.foreach{ i =>
        if (i <= animCursor){
          tex.draw(fireTex)
          glTranslatef(slotSize.x, 0, 0)
        }
      }
      glPopMatrix()
    }
  }

  def summonSpell(card : Card) = shift { k: (Int => Unit) =>
    if (card.houseIndex == 0 && card.cost == 6) {
      panel.addTask(new FlameAnimTask(k))
    } else k(0)
  }

  def listenEvent(slotButton: SlotButton) {
    slotButton.on {
      case MouseClicked(_) if slotButton.enabled =>
        game.commandRecorder.addInput(new SlotInput(slotButton.num))
    }
  }
  def setSlotEnabled(empty: Boolean) {
    slots.foreach { slot => slot.enabled = (slot.isEmpty == empty) }
  }
  def disable() { slots.foreach(_.enabled = false) }
  def refresh() {
    lifeLabel.life.refresh()
    slots.foreach(_.refresh())
  }
}
