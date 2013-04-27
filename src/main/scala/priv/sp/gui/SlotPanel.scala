package priv.sp.gui

import priv._
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL20._
import priv.sp._
import scala.util.continuations._
import scala.util.Random

class SlotPanel(playerId : PlayerId, val game : Game) {
  val lifeLabel = new LifeLabel(game.names(playerId), new DamagableInt(game.state.players(playerId).life, game), game)
  val slots =
    slotRange.map(num =>
      new SlotButton(
        num,
        playerId,
        game.state.players(playerId).slots.get(num),
        game)).toList
  val elts = lifeLabel :: /**testButton ::: */ slots

  def testButton = (if (playerId == game.myPlayerId) List(TestButton(game.sp)) else Nil)

  val slotOffset = lifeLabel.size
  val slotSize = slots(0).size
  val slotCenter = slotSize * 0.5
  slots.foreach(listenEvent _)

  val panel = Row(elts)

  class SpellAnim(lock : AnyRef, entity : TimedEntity, isRelative : Boolean = true) extends Task[Unit] {
    private val attach = if (isRelative) panel else game.world
    def duration = entity.duration
    def init() { attach.spawn(entity)  }
    def end() = {
      attach.unspawn(entity)
      lock.synchronized{lock.notifyAll()}
    }
  }

  def summonSpell(command : Command, sourceCoord : Coord2i, lock : AnyRef) = {
    import command.card
    def absTargetSlotCoord = command.input.map{ slotInput =>
      (slots(0).coord.xProj + (slotSize.x * slotInput.num)) + slotCenter
    }
    if (card.houseIndex == 0 && card.cost == 6) {
      panel.addTask(new SpellAnim(lock, new Flame(game.sp, slotOffset, slotSize)))
    } else if (card.houseIndex == 2 && card.cost == 3) {
      panel.addTask(
        new SpellAnim(lock,
          isRelative = false,
          entity = new Lightning(game.sp, sourceCoord, absTargetSlotCoord.get, slots(0).coord)))
    } else if (card.houseIndex == 2 && card.cost == 6) {
      panel.addTask(
        new SpellAnim(lock, isRelative = false,
          entity = new Lightning(game.sp, sourceCoord, slots(0).coord)))
    } else if (card.houseIndex == 2 && card.cost == 8) {
      val points = (List(slots(5).coord + slotCenter) /: slots.reverse){ (acc, slot) =>
        if (!slot.isEmpty){
          val deviation = if (acc.size > 1) ((acc.size % 2) -0.5) * 20 else 0
          ((slots(slot.num).coord + slotCenter).yProj + deviation) :: acc
        } else acc
      }
      panel.addTask(
        new SpellAnim(lock, isRelative = false,
          entity = new Lightning(game.sp, points.reverse :+ lifeLabel.coord.yProj + slotCenter.y : _*)))
    } else lock.synchronized(lock.notifyAll())
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

import Coord2i._

class Flame(sp : SpWorld, slotOffset : Coord2i, slotSize : Coord2i) extends TimedEntity {
  val duration = 1500L
  val fireTex = sp.baseTextures.fire
  val offset = Coord2i(slotOffset.x + slotSize.x / 2,  slotSize.y / 2)
  val partTimeLine = slotRange.toList.flatMap{ numSlot =>
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

  def render() {
    glBlendFunc(GL_SRC_ALPHA, GL_ONE)
    val delta = getDelta()
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

class Lightning(sp : SpWorld, points : Coord2i*) extends TimedEntity {
  val posInterval = new FollowLines(points.zip(points.tail).map{ case (p1, p2) =>
    new SegInterval(p1, p2)
  })
  val duration = posInterval.duration
  val ctTex = sp.baseTextures.callthunder
  val trailRange = 0 to 10

  def render() {
    glBlendFunc(GL_SRC_ALPHA, GL_ONE)
    trailRange.foreach{ x =>
      glColor4f(1, 1, 1, 1f/(1+x))
      val size = ctTex.size * (1f/(1+x))
      tex.drawAt(recenter(posInterval.posAt(getDelta() - (x * 5)), size), ctTex.id, size)
    }
  }

  class SegInterval(a : Coord2i, b :Coord2i){
    val speed = 3/2f
    val distance = math.sqrt(sqrDist(a, b))
    val duration = (distance / speed).toLong

    def posAt(t : Long) = a + ((Coord2i(b.x - a.x, b.y - a.y) * (t.floatValue /duration)))
  }

  class FollowLines(l : Traversable[SegInterval]) {
    val duration = l.map(_.duration).sum
    def posAt(t : Long) = {
      val (offset, line) = ((0L, l.head) /: l.tail){ case ((acc, last), x) =>
        if (acc + last.duration < t) {
          (acc + last.duration, x)
        } else (acc, last)
      }
      line.posAt(t - offset)
    }
  }
}
