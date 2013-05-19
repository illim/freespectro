package priv.sp.update

import collection._
import priv.sp._
import priv.util.FieldUpdate

// crap most functions suppose value is tested if well defined
class SlotUpdate(val num : Int, val slots : SlotsUpdate) extends FieldUpdate(Some(slots), slots.value.get(num)){
  import slots.{player, updater}
  import player.id
  private lazy val attackUpdate = new AttackUpdate(this)
  lazy val adjacentSlots = adjacents(num).map{ n => slots(n) }

  @inline def get = value.get
  def toggleRun()             { write(value.map(x => x.copy(status = CardSpec.runFlag))) }
  def toggle(flag : Int)      { write(value.map(x => x.copy(status = x.status | flag))) }
  def setData(data : AnyRef)  { write(value.map(_.copy( data = data))) }
  def heal(amount : Int)      { write(value.map(x => SlotState.addLife(x, amount)))  }
  def inflict(damage : Damage){ if (value.isDefined) damageSlot(player.mod(damage))  }
  def attack = attackUpdate.reinit()
  def apply() = updated {
    value.map{ s =>
      if (attackUpdate.isDirty){
        val attackSources = attackUpdate.value
        s.copy(attack = getAttack(attackSources), attackSources = attackSources)
      } else s
    }
  }

  def add(card : Creature) : SlotState = {
    add(SlotState(card, card.life, card.status, card.attack, getAttack(card.attack), card.data))
  }

  def add(slot : SlotState) : SlotState = {
    write(Some(slot))
    slots.reactAdd(this)
    value.get
  }

  def getAttack(attackSources : AttackSources) = {
    (attackSources.base.getOrElse(0) /: attackSources.sources){ (acc, s) =>
      s match {
        case f : AttackFunc => f(acc)
        case f : AttackStateFunc => f(acc, player)
        case _ => acc
      }
    }
  }

  def damageSlot(damage : Damage) = {
    if (value.isDefined) {
      val d = slots.protect(num, damage) // /!\ possible side effect
      get.inflict(d) match {
        case None          => delayedDestroy(d)
        case Some(newslot) => write(Some(newslot))
      }
    }
  }

  def destroy(){
    val card = get.card
    remove()
    slots.onDead(Dead(num, card, player, isEffect = true))
  }

  def remove(){
    val slotState = get
    attackUpdate.invalidate() // FIXME hack?
    write(None)
    slotState.card.reaction.onRemove(this)
  }

  def focus(blocking : Boolean = true){
    slots.updateListener.focus(num, id, blocking)
  }

  private def delayedDestroy(d : Damage){
    val card = get.card
    remove()
    slots.log(Dead(num, card, player, isEffect = d.isEffect))
  }
}

class AttackUpdate(slot : SlotUpdate) extends FieldUpdate(Some(slot), slot.value.map(_.attackSources).get) {
  private val some0 = Option(0)

  def add(source : AttackSource)   { if (value.base != some0) write(value.add(source))  }
  def remove(source : AttackSource){ write(value.remove(source))  }
  def has[A : reflect.ClassTag] = value.sources.exists{
    case _ : A => true
    case _ => false
  }
}
