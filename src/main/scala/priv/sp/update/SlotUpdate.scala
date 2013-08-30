package priv.sp.update

import collection._
import priv.sp._
import priv.util.FieldUpdate

// crap most functions suppose value is tested if well defined
class SlotUpdate(val num : Int, val slots : SlotsUpdate) extends FieldUpdate(Some(slots), slots.value.get(num)){
  val updater = slots.updater
  val player = slots.player
  val playerId = player.id
  val otherPlayerId = other(playerId)
  val attackUpdate = new AttackUpdate(this)
  lazy val adjacentSlots : List[SlotUpdate] = adjacents(num).map{ n => slots(n) }
  lazy val otherHouseListener = updater.houseEventListeners(otherPlayerId)
  def otherPlayer = updater.players(otherPlayerId)

  def oppositeSlot = otherPlayer.slots(num)
  def filledAdjacents = adjacentSlots.filter(_.value.isDefined)
  def oppositeState : Option[SlotState] = otherPlayer.getSlots.get(num)

  @inline def get = value.get
  // some crap
  def toggleRun() {
    value.foreach{ x =>
      if (!x.has(CardSpec.runFlag) || x.has(CardSpec.stunFlag)) {
        write(value.map(x => x.copy(status = (x.status | CardSpec.runFlag) & (~ CardSpec.stunFlag))))
      }
    }
  }
  def toggle(flag : Int)      { write(value.map(x => x.copy(status = x.status | flag))) }
  def toggleOff(flag : Int)   { write(value.map(x => x.copy(status = x.status & (~ flag)))) }
  def setData(data : AnyRef)  { write(value.map(_.copy( data = data))) }
  def setTarget(target : List[Int]) { write(value.map(_.copy( target = target))) }

  // -- code horror to intercept heal/inflict
  def heal(amount : Int)      { if (value.isDefined) get.reaction.heal(amount) }
  def inflict(damage : Damage){
    if (value.isDefined) get.reaction.inflict(player.mod(damage))
  }
  def destroy() { if (value.isDefined){ get.reaction.destroy() } }
  def stun()    { if (value.isDefined){ get.reaction.stun() } }
  // --

  def attack = attackUpdate.reinit()
  def apply() = updated {
    value.map{ s =>
      if (attackUpdate.isDirty){
        val attackSources = attackUpdate.value.get
        s.copy(attack = slots.getAttack(this, attackSources), attackSources = attackSources)
      } else s
    }
  }

  def add(card : Creature) {  add(slots.buildSlotState(this, card)) }
  val add = new priv.util.ObservableFunc1({ slot : SlotState =>
    write(Some(slot))
    slot.reaction.use(this)
    slots.reactAdd(this)
  })

  // /!\ don't call it directly (use inflict)
  def damageSlot(damage : Damage) = {
    if (value.isDefined) {
      val slotState = get
      val d = protect(slotState.reaction.selfProtect(damage)) // /!\ possible side effect (jf can protect herself once and toggle a flag)
      val slot = get
      val amount = slot.inflict(d) match {
        case None =>
          delayedDestroy(d)
          slot.life
        case Some(newslot) =>
          write(Some(newslot))
          slot.life - newslot.life
      }
      slotState.reaction.onMyDamage(amount)
      slots.player.updater.houseEventListeners.foreach(_.onDamaged(slot.card, amount, this))
    }
  }

  val protect = new priv.util.InterceptableFunc1({d : Damage =>
    d
  })

  def privDestroy(){
    val s = get
    val event = Dead(num, s, player, None)
    remove(Some(event))
    slots.onDead(event)
  }

  def remove(deadOpt : Option[Dead] = None) : SlotState = {
    val slotState = get
    slotState.reaction.onMyRemove(deadOpt)
    slots.reactRemove(this)
    val result = apply().get
    attackUpdate.invalidate() // FIXME hack?
    slotState.reaction.cleanUp()
    write(None)
    result
  }

  def focus(blocking : Boolean = true){
    slots.updateListener.focus(num, playerId, blocking)
  }

  private def delayedDestroy(d : Damage){
    val s = get
    val event = Dead(num, s, player, Some(d))
    remove(Some(event))
    slots.log(event)
  }
}

class AttackUpdate(slot : SlotUpdate) extends FieldUpdate(Some(slot), slot.value.map(_.attackSources)) {
  private val some0 = Option(0)

  @inline def get = value.get
  def add(source : AttackSource)        { if (get.base != some0) write(Some(get.add(source)))  }
  def removeFirst(source : AttackSource){ write(Some(get.removeFirst(source)))  }
  def removeAny(source : AttackSource)  { write(Some(get.removeAny(source)))  }
  def has[A : reflect.ClassTag] = get.sources.exists{
    case _ : A => true
    case _ => false
  }
}
