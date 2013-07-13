package priv.sp.update

import collection._
import priv.sp._
import priv.util.FieldUpdate

// crap most functions suppose value is tested if well defined
class SlotUpdate(val num : Int, val slots : SlotsUpdate) extends FieldUpdate(Some(slots), slots.value.get(num)){
  import slots.{player, updater}
  val playerId = player.id
  private lazy val attackUpdate = new AttackUpdate(this)
  lazy val adjacentSlots : List[SlotUpdate] = adjacents(num).map{ n => slots(n) }
  lazy val otherHouseListener = updater.houseEventListeners(other(playerId))

  def oppositeSlot = player.otherPlayer.slots(num)
  def filledAdjacents = adjacentSlots.filter(_.value.isDefined)

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
  def heal(amount : Int)      { write(value.map(x => SlotState.addLife(x, amount)))  }
  def inflict(damage : Damage){ if (value.isDefined) damageSlot(player.mod(damage))  }
  def attack = attackUpdate.reinit()
  def apply() = updated {
    value.map{ s =>
      if (attackUpdate.isDirty){
        val attackSources = attackUpdate.value
        s.copy(attack = slots.getAttack(this, attackSources), attackSources = attackSources)
      } else s
    }
  }

  def add(card : Creature) {  add(slots.buildSlotState(this, card)) }
  def add(slot : SlotState) {
    write(Some(slot))
    slots.reactAdd(this)
    updater.houseEventListeners.foreach(_.onAdd(this))
  }

  def damageSlot(damage : Damage) = {
    if (value.isDefined) {
      val card = get.card
      val d = otherHouseListener.protectOpp(
        this,
        player.houseEventListener.protect(
          this,
          card.reaction.selfProtect(damage, this))) // /!\ possible side effect (jf can protect herself once and toggle a flag)
      val slot = get
      val amount = slot.inflict(d) match {
        case None =>
          delayedDestroy(d)
          slot.life
        case Some(newslot) =>
          write(Some(newslot))
          slot.life - newslot.life
      }
      card.reaction.onMyDamage(amount, this)
      slots.player.updater.houseEventListeners.foreach(_.onDamaged(slot.card, amount, this))
    }
  }

  def destroy(){
    if (value.isDefined){ // crap for marine
      val s = get
      val event = Dead(num, s, player, None)
      remove(Some(event))
      slots.onDead(event)
    }
  }

  def remove(deadOpt : Option[Dead] = None){
    val slotState = get
    slotState.card.reaction.onMyRemove(this, deadOpt)
    write(None)
    attackUpdate.invalidate() // FIXME hack?
  }

  def focus(blocking : Boolean = true){
    slots.updateListener.focus(num, playerId, blocking)
  }

  def update(f : SlotState => SlotState){
    write(value.map(f))
  }

  private def delayedDestroy(d : Damage){
    val s = get
    val event = Dead(num, s, player, Some(d))
    remove(Some(event))
    slots.log(event)
  }
}

class AttackUpdate(slot : SlotUpdate) extends FieldUpdate(Some(slot), slot.value.map(_.attackSources).get) {
  private val some0 = Option(0)

  def add(source : AttackSource)   { if (value.base != some0) write(value.add(source))  }
  def removeFirst(source : AttackSource){ write(value.removeFirst(source))  }
  def removeAny(source : AttackSource){ write(value.removeAny(source))  }
  def has[A : reflect.ClassTag] = value.sources.exists{
    case _ : A => true
    case _ => false
  }
}
