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
  def getEmptySlots = { ensureInited(); slotUpdates.filter(_.value.isEmpty) }
  def apply(n : Int) = slots(n)
  def ensureInited() = if (!first.isInited){ slotUpdates.foreach(_.reinit()) }

  def inflictCreatures(damage : Damage, playerId : PlayerId) {
    val d = mod(damage, playerId)
    foreach( _.damageSlot(d))
  }

  def summon(num : Int, card : Creature) {
    val slot = slots(num)
    slot.value.foreach{ s =>
      s.card.reaction.onOverwrite(card, slot)
      card.reaction.onSpawnOver(slot)
    }
    val slotState = buildSlotState(card)
    updateListener.summon(num, slotState, id) // bit fake for altar
    if (slot.value.isEmpty) {
      slot.add(slotState)
      val summonEvent = SummonEvent(num, card, player)
      otherPlayer.slots.reactSummon(summonEvent)
      reactSummon(summonEvent)
    }
  }

  def move(num : Int, dest : Int){
    val slot = slots(num)
    slot.value.foreach{ s =>
      // HACK (have to recreate the slot to recalcul attack)
      updateListener.move(num, dest, id)
      slot.remove()
      slots(dest).add(
        SlotState(s.card, s.life, s.status, s.card.attack, getAttack(s.card.attack) , s.data))
    }
  }

  def protect(num : Int, damage : Damage) = {
    foldl(damage) { (acc, s) =>
      s.get.card.reaction.onProtect(s.num, DamageEvent(acc, Some(num), player, None))
    }
  }

  def onDead(dead : Dead){
    updateListener.die(dead.num, player.id)
    dead.card.reaction.onMyDeath(dead)
    foreach{ s =>
      if (s.num != dead.num) s.get.card.reaction.onDeath(s.num, dead)
    }
  }
  def buildSlotState(card : Creature) = SlotState(card, card.life, card.status, card.attack, getAttack(card.attack), card.data)
  def getAttack(attackSources : AttackSources) = {
    (attackSources.base.getOrElse(0) /: attackSources.sources){ (acc, s) =>
      s match {
        case f : AttackFunc => f(acc)
        case f : AttackStateFunc => f(acc, player)
        case _ => acc
      }
    }
  }
  def toggleRun() = foreach(_.toggleRun())
  def healCreatures(amount : Int) = foreach(_.heal(amount))
  def reactSummon(e : SummonEvent) = foreach { s =>
    if (player.id != e.player.id || s.num != e.num){
      s.get.card.reaction.onSummon(s.num, id, e)
    }
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
