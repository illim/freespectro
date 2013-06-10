package priv.sp.house

import priv.sp._
import priv.sp.update._
import CardSpec._
import GameCardEffect._

class Warp {

  val Warp = House("Warp", List(
    Creature("Errant", Attack(4), 19, "Hide from the world after killing a creature, come back when damaged.", runAttack = new ErrantAttack, reaction = new ErrantReaction),
    Spell("EarthQuake", "Deals to opponent creatures damage equals to their mana", effects = effects(Direct -> quake)),
    Creature("Cloak", Attack(4), 18, "When die restore the creature.(Can't cloak himself)", inputSpec = Some(SelectOwnerCreature), reaction = new CloakReaction),
    Creature("Photographer", Attack(3), 18, "If there's already a photograph, owner slots is reverted to the state\nwhen last one was spawned", effects = effects(Direct -> photo)),
    Creature("WatchMaker", Attack(6), 25, "When summoned, opposite opposite creature lose his abilities until watchmaker die.", reaction = new WMReaction),
    Creature("Ram", Attack(6), 26, "Opposite creature is destroyed and opponent get his mana back -2.", effects = effects(Direct -> ram)),
    Creature("Stranger", AttackSources().add(new StrangerAttack), 35, "Attack is highest opponent mana. When summoned, take effects of opposite slot.", effects = effects(Direct -> merge)),
    Creature("WarpQueen", Attack(6), 35, "Opponent creatures lose their ability until end of next turn.\nDeals 5 damage to each of them", effects = effects(Direct -> warp))))

  val photographer = Warp.cards(3)
  Warp.initCards(Houses.basicCostFunc)

  def quake = { env : Env =>
    import env._
    otherPlayer.slots.foreach{ slot =>
      val d = Damage(otherPlayer.getHouses(slot.get.card.houseIndex).mana, env, isSpell = true)
      slot.inflict(d)
    }
  }
  def merge = { env : Env =>
    import env._
    val oppSlot = otherPlayer.slots(selected)
    oppSlot.value.foreach{ opp =>
      val slot = player.slots(selected)
      val s = slot.get
      slot.destroy()
      slot.add(SlotState(new MergeStranger(s.card , opp.card), s.life, s.status, s.attackSources, player.slots.getAttack(s.attackSources), s.data))
    }
  }
  def photo : Effect = { env : Env =>
    import env._
    player.slots.findCard(photographer) match {
      case Some(s) if s.get.data != null =>
        val backup = s.get.data.asInstanceOf[PlayerState.SlotsType]
        player.slots.slots.foreach{ s =>
          s.write(backup.get(s.num))
        }
      case _ =>
        val slot = player.slots(selected)
        slot.setData(player.getSlots - selected)
    }
  }
  def ram = { env : Env =>
    import env._
    val oppSlot = otherPlayer.slots(selected)
    oppSlot.value.foreach{ s =>
      oppSlot.destroy()
      otherPlayer.houses.incrMana(math.max(0, s.card.cost - 2), s.card.houseIndex)
    }
  }
  def warp = { env : Env =>
    import env._
    otherPlayer.slots.slots.collect{ case slot if slot.value.isDefined =>
      val s = slot.get
      slot.remove()
      (s, slot)
    }.foreach{ case (s, slot) => bridle(s, slot) }
    otherPlayer.slots.inflictCreatures(Damage(5, env, isAbility = true))
    player.addEffect(OnEndTurn -> new CountDown(2, { env : Env =>
      env.otherPlayer.slots.foreach(unbridle)
    }))
  }
  def bridle(s : SlotState, slot : SlotUpdate){
    slot.write(Some(SlotState(new MereMortal(s.card), s.life, s.status, s.attackSources, slot.slots.getAttack(s.attackSources), s.data)))
  }
  def unbridle(slot : SlotUpdate) {
    slot.value.foreach{ s =>
      s.card match {
        case m : MereMortal =>
          slot.remove()
          slot.add(SlotState(m.c, s.life, s.status, s.attackSources, slot.slots.getAttack(s.attackSources), s.data))
        case _ =>
      }
    }
  }
  class WMReaction extends Reaction {
    override def onAdd(selected : Int, slot : SlotUpdate) {
      if (selected == slot.num){
        val oppSlot = slot.slots.player.otherPlayer.slots(selected)
        oppSlot.value.foreach{ s =>
          oppSlot.remove()
          bridle(s, oppSlot)
        }
      }
    }
    override def onRemove(slot : SlotUpdate) {
      unbridle(slot.slots.player.otherPlayer.slots(slot.num)) // FIXME bugged between wm and wq
    }
  }
}

class ErrantAttack extends RunAttack {

  def apply(num : Int, d : Damage, player : PlayerUpdate) {
    val otherPlayer = player.otherPlayer
    val slot = otherPlayer.slots(num)
    if (slot.value.isEmpty) {
      otherPlayer.inflict(d)
    } else {
      slot.inflict(d)
      if (slot.value.isEmpty){
        player.slots(num).toggle(CardSpec.pausedFlag)
      }
    }
  }
}

class ErrantReaction extends Reaction {
  override def onMyDamage(amount : Int, slot : SlotUpdate){
    slot.value.foreach{ s =>
      if (s.has(pausedFlag)){
        slot.toggleOff(pausedFlag)
      }
    }
  }
}

class StrangerAttack extends AttackStateFunc {
  def apply(attack : Int, player : PlayerUpdate) : Int = {
    attack + player.otherPlayer.getHouses.maxBy(_.mana).mana
  }
}

class CloakReaction extends Reaction {
  override def onSpawnOver(slot : SlotUpdate) = {
    val s = slot.get
    slot.remove()
    Some(new CloakSlotMod(s))
  }

  override def onMyDeath(dead : Dead){
    import dead._
    val cloaked = dead.slot.data.asInstanceOf[SlotState]
    if (cloaked != null){
      val slot = player.slots(num)
      slot.add(cloaked.card)
      slot.update(_.copy(life = cloaked.life))
    }
  }
}

class CloakSlotMod(cloaked : SlotState) extends SlotMod {
  def apply(slotState : SlotState) = {
    slotState.copy(data = cloaked)
  }
}

class MereMortal(val c : Creature)
extends Creature(c.name, c.attack, c.life, status = c.status){
  houseId = c.houseId
  houseIndex = c.houseIndex
  cost = c.cost
  id = c.id
}

class MergeStranger(s : Creature, c : Creature)
extends Creature(
  s.name,
  s.attack,
  s.life,
  s.description,
  s.inputSpec,
  c.effects,
  c.mod,
  c.reaction,
  c.data,
  if (c.runAttack == MultiTargetAttack) s.runAttack else c.runAttack,
  c.immune,
  c.isAltar,
  c.status){
  houseId = s.houseId
  houseIndex = s.houseIndex
  cost = s.cost
  id = s.id
}
