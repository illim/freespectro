package priv.sp.gui

import priv._
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL20._
import priv.sp._
import scala.util.continuations._
import scala.util.Random

class SlotPanel(playerId : PlayerId, val game : Game) extends SpellAnims {
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

  abstract class SpellAnim(k : Int => Unit) extends Task[Unit] { _ : Entity =>
    def init() { panel.entities.add(this)  }
    def end() = {
      panel.unspawn(this)
      k(duration.intValue)
    }
  }

  def summonSpell(card : Card) = shift { k: (Int => Unit) =>
    if (card.houseIndex == 0 && card.cost == 6) {
      panel.addTask(new SpellAnim(k) with FlameAnim)
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

trait SpellAnims { _ : SlotPanel =>

  // Very ugly anim
  trait FlameAnim extends Entity { _ : Task[Unit] =>
    import Coord2i._

    val duration = 1500L
    val fireTex = game.sp.baseTextures.fire
    val offset = Coord2i(slotOffset.x + slotSize.x / 2,  slotSize.y / 2)
    val partTimeLine = slotRange.toList.flatMap{ numSlot =>
      val slotTime = numSlot * 150
      val nbPart = 4 + Random.nextInt(2)
      val slotOffset = offset.xProj + (slotSize.x * numSlot)
      (List.empty[(Int, Coord2i)] /: (0 to nbPart)){ (acc, n) =>
        val xf = 1 + n * (40/nbPart)
        (Random.nextInt(5), slotOffset + Coord2i(Random.nextInt(xf) - xf/2, Random.nextInt(10) - 5)) :: acc
      }
    }
    var currentPart = 0
    var shownParts = List.empty[(Coord2i, Long)]
    var nbPart = partTimeLine.size

    def render(world: World) {
      glBlendFunc(GL_SRC_ALPHA, GL_ONE)
      val delta = deltaT(world.time)
      if (currentPart < nbPart && delta > partTimeLine(currentPart)._1){
        shownParts = (partTimeLine(currentPart)._2, delta) :: shownParts
        currentPart += 1
      }
      shownParts.foreach { case (p, d) =>
        val cursor = delta - d
        val fact = (cursor/20f).intValue
        val k = math.cos(cursor/500f).floatValue
        glColor4f(k, k, k, k)
        tex.drawAt(p.yProj.-(fact), fireTex.id, fireTex.size.yProj + fact)
      }
    }
  }
}
