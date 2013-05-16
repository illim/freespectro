package priv.sp.update

import collection._
import priv.sp._
import priv.util.FieldUpdate

class SlotsUpdate(val player : PlayerUpdate) extends FieldUpdate(Some(player), player.value.slots){
  import player._
  var logs = List.empty[BoardEvent]
  private val slotUpdates = slotRange.map{ i => new SlotUpdate(i, this) }.toVector
  private val first = slotUpdates(0)

  def updater = player.updater
  def updateListener = updater.updateListener

  def apply() = updated {
    var res = PlayerState.emptySlots
    slotUpdates.foreach{ slot =>
      slot().foreach{ s =>
        res += (slot.num -> s)
      }
    }
    res
  }

  def slots = { ensureInited(); slotUpdates }
  def filleds = { ensureInited(); slotUpdates.filter(_.value.isDefined) }
  def apply(n : Int) = slots(n)
  def ensureInited() = if (!first.isInited){ slotUpdates.foreach(_.reinit()) }

  def inflictCreatures(damage : Damage) {
    val d = mod(damage)
    foreach( _.damageSlot(d))
  }

  // todo move it to slot
  def summon(num : Int, creature : Creature) {
    val slot = slots(num).add(creature)
    updateListener.summon(num, slot, id)
    val summonEvent = SummonEvent(num, creature, id, updater)
    otherPlayer.slots.reactSummon(summonEvent)
    reactSummon(summonEvent)
  }

  def move(num : Int, dest : Int){
    val slot = slots(num)
    slot.value.foreach{ s =>
      // HACK (have to recreate the slot to recalcul attack)
      updateListener.move(num, dest, id)
      slot.remove()
      slots(dest).add(
        SlotState(s.card, s.life, s.status, s.card.attack, slot.getAttack(s.card.attack) , s.data))
    }
  }

  def protect(num : Int, damage : Damage) = {
    foldl(damage) { (acc, s) =>
      s.get.card.reaction.onProtect(s.num, DamageEvent(acc, Some(num), id, updater, None))
    }
  }

  def onDead(dead : Dead){
    dead.card.reaction.onMyDeath(dead)
    foreach{ s =>
      if (s.num != dead.num) s.get.card.reaction.onDeath(s.num, dead)
    }
  }

  def toggleRun() = foreach(_.toggleRun())
  def healCreatures(amount : Int) = foreach(_.heal(amount))
  def reactSummon(summonEvent : SummonEvent) = foreach { s =>
    s.get.card.reaction.onSummon(s.num, id, summonEvent)
  }
  def reactAdd(slot : SlotUpdate) = foreach { s =>
    s.get.card.reaction.onAdd(s.num, slot)
  }
  def log(evt : BoardEvent){ logs = evt :: logs }
  def foreach(f : SlotUpdate => Unit) = slots.foreach{ s =>
    if (s.value.isDefined){
      f(s)
    }
  }
  def foldl[B](z : B)(f : (B, SlotUpdate) => B) = {
    var r = z
    foreach{ s => r = f(r, s) }
    r
  }
  def findCard(card : Card) : Option[SlotUpdate] = {
    slots.find{ s =>
      s.value.isDefined && s.get.card == card
    }
  }
}
