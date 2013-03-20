package priv.sp.gui

import priv._
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL20._
import priv.sp._
import scala.util.continuations._
import scala.util.Random

class SlotPanel(playerId : PlayerId, val game : Game) {
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

  class SpellAnim(k : Int => Unit, entity : SpellEntity, isRelative : Boolean = true) extends Task[Unit] {
    private val attach = if (isRelative) panel else game.world
    def duration = entity.duration
    def init() { attach.entities.add(entity)  }
    def end() = {
      attach.unspawn(entity)
      k(duration.intValue)
    }
  }

  def summonSpell(command : Command, sourceCoord : Coord2i) = shift { k: (Int => Unit) =>
    import command.card
    val absTargetSlotCoord = command.input.map{ slotInput =>
      (slots(0).coord.xProj + (slotSize.x * slotInput.num)) + (slotSize * 0.5)
    }
    if (card.houseIndex == 0 && card.cost == 6) {
      panel.addTask(new SpellAnim(k, new Flame(game.sp, slotOffset, slotSize)))
    } else if (card.houseIndex == 2 && card.cost == 3) {
      panel.addTask(new SpellAnim(k, new CallThunder(game.sp, slots(0).coord, sourceCoord, absTargetSlotCoord.getOrElse(slots(0).coord)), isRelative = false))
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

trait SpellEntity extends Entity {
  def duration : Long
}

import Coord2i._

// Very ugly anim
class Flame(sp : SpWorld, slotOffset : Coord2i, slotSize : Coord2i) extends SpellEntity {
  val duration = 1500L
  val fireTex = sp.baseTextures.fire
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
      val size = fireTex.size.yProj + fact
      tex.drawAt(recenter(p.yProj - fact, size), fireTex.id, size)
    }
  }
}

class CallThunder(sp : SpWorld, slotOffset : Coord2i, sourceCoord: Coord2i, targetSlotCoord : Coord2i) extends SpellEntity {
  val duration = 1000L
  val ctTex = sp.baseTextures.callthunder
  val sourceToSlot = new FollowLine(sourceCoord, targetSlotCoord, 500)
  val slotToOffset = new FollowLine(targetSlotCoord, slotOffset, 500)
  val trailRange = 0 to 10

  def render(world: World) {
    glColor4f(1, 1, 1, 1)
    glBlendFunc(GL_SRC_ALPHA, GL_ONE)
    val delta = deltaT(world.time)
    trailRange.foreach{ x =>
      val size = ctTex.size * (1f/(1+x))
      tex.drawAt(recenter(getPos(delta - (x * 5)), size), ctTex.id, size)
    }
  }

  def getPos(t : Long) = {
    if (t > sourceToSlot.duration){
      slotToOffset.posAt(t - sourceToSlot.duration)
    } else sourceToSlot.posAt(t)
  }
  class FollowLine(a : Coord2i, b :Coord2i, val duration : Long){
    def posAt(t : Long) = a + ((Coord2i(b.x - a.x, b.y - a.y) * (t.floatValue/duration)))
  }
}
