package priv.sp.gui

import priv._
import priv.sp._
import priv.sp.gui.spell._

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

  //def testButton = (if (playerId == game.myPlayerId) List(TestButton(game.sp)) else Nil)

  val slotOffset = lifeLabel.size
  val slotSize = slots(0).size
  val slotCenter = slotSize * 0.5
  slots.foreach(listenEvent _)

  val panel = Row(elts)

  class SpellAnim(lock : AnyRef, entity : TimedEntity, isRelative : Boolean = true, blocking : Boolean = true) extends Task[Unit] {
    private val attach = if (isRelative) panel else game.world
    def duration = entity.duration
    def init() {
      if (!blocking){
        unlock()
      }
      attach.spawn(entity)
    }
    def end() = {
      attach.unspawn(entity)
      if (blocking){
        unlock()
      }
    }
    def unlock(){lock.synchronized{lock.notifyAll()}}
  }

  import game.sp.houses._

  def summonSpell(command : Command, sourceCoord : Coord2i, lock : AnyRef) = {
    import command.card
    def absTargetSlotCoord = command.input.map{ slotInput =>
      (slots(0).coord.xProj + (slotSize.x * slotInput.num)) + slotCenter
    }
    if (card == Fire.cards(5)) {         panel.addTask( new SpellAnim(lock, new Flame(game.sp, slotOffset, slotSize)))
    } else if (card == Water.cards(7)) { panel.addTask( new SpellAnim(lock, new AcidRain(game.sp)))
    } else if (card == Earth.cards(8)) { panel.addTask( new SpellAnim(lock, new StoneRain(game.sp)))
    } else if (card == Air.cards(2)) {
      panel.addTask(
        new SpellAnim(lock,
          isRelative = false,
          entity = new Lightning(game.sp, sourceCoord, absTargetSlotCoord.get, slots(0).coord)))
    } else if (card == Earth.cards(1)) {
      panel.addTask(
        new SpellAnim(lock,
          isRelative = false,
          entity = new NatureRitual(absTargetSlotCoord.get, game.sp)))
    } else if (card == sower.Sower.cards(3)) {
      panel.addTask(
        new SpellAnim(lock,
          isRelative = false,
          blocking = false,
          entity = new Pollinate(absTargetSlotCoord.get, game.sp)))
    } else if (card == Air.cards(5)) {
      panel.addTask(
        new SpellAnim(lock, isRelative = false,
          entity = new Lightning(game.sp, sourceCoord, slots(0).coord)))
    } else if (card == Air.cards(7)) {
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
