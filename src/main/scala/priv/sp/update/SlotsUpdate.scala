package priv.sp.update

import collection._
import priv.sp._
import priv.util.FieldUpdate

class SlotsUpdate(val player : PlayerUpdate) extends FieldUpdate(Some(player), player.value.slots){
  import player._
  var logs = List.empty[BoardEvent]
  private val slotUpdates = baseSlotRange.map{ i => new SlotUpdate(i, this) }.toVector
  private val first = slotUpdates(0)
  val playerId = player.id

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
  def getOpenSlots = { ensureInited(); slotUpdates.filter(s => s.value.isEmpty && player.value.isInSlotRange(s.num)) }
  def apply(n : Int) = slots(n)
  def ensureInited() = if (!first.isInited){ slotUpdates.foreach(_.reinit()) }

  def inflictCreatures(damage : Damage) {
    val d = mod(damage)
    foreach( _.damageSlot(d))
  }

  def summon(num : Int, card : Creature) {
    val slot = slots(num)
    var slotState = buildSlotState(card)
    slot.value.foreach{ s =>
      s.card.reaction.onOverwrite(card, slot)
      card.reaction.onSpawnOver(slot).foreach{ m =>
        slotState = m(slotState)
      }
    }
    updateListener.summon(num, slotState, id) // bit fake for altar
    if (slot.value.isEmpty) {
      slot.add(slotState)
      val summonEvent = SummonEvent(num, card, player)
      otherPlayer.slots.reactSummon(summonEvent)
      reactSummon(summonEvent)
    }
  }

  def move(num : Int, dest : Int){
    if (player.value.isInSlotRange(dest)){
      val slot = slots(num)
      slot.value.foreach{ s =>
        updateListener.move(num, dest, id)
        slot.remove()
        if (slots(dest).value.isDefined){
          move(dest, num)
        }
        slots(dest).add(
          SlotState(s.card, s.life, s.status, s.card.attack, getAttack(s.card.attack) , s.data))
      }
    }
  }

  def onDead(dead : Dead){
    updateListener.die(dead.num, playerId)
    dead.card.reaction.onMyDeath(dead)
    player.updater.houseEventListeners.map(_.onDeath(dead))
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
    if (playerId != e.player.id || s.num != e.num){
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
  def reduce(f : (SlotUpdate, SlotUpdate) => SlotUpdate) : Option[SlotUpdate] = {
    foldl(Option.empty[SlotUpdate]){
      case (None, s) => Some(s)
      case (Some(s0), s1) => Some(f(s0, s1))
    }
  }
  def findCard(card : Card) : Option[SlotUpdate] = {
    slots.find{ s =>
      s.value.isDefined && s.get.card == card
    }
  }
}
