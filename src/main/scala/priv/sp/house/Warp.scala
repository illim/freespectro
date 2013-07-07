package priv.sp.house

import priv.sp._
import priv.sp.update._
import CardSpec._
import GameCardEffect._

// FIXME: schizo when unbridle
class Warp {

  val Warp = House("Warp", List(
    Creature("Errant", Attack(4), 19, "Hide in shadow after killing a creature, come back when damaged.", runAttack = new ErrantAttack, reaction = new ErrantReaction),
    Spell("EarthQuake", "Deals to opponent creatures damage equals to their mana", effects = effects(Direct -> quake)),
    Creature("Cloak", Attack(4), 18, "When die restore the creature.", inputSpec = Some(SelectOwnerCreature), reaction = new CloakReaction),
    Creature("Photographer", Attack(3), 20, "If there's already a photographer, owner empty slots are reverted to the state\nwhen the other was summoned.\nOld photographer is destroyed", effects = effects(Direct -> photo)),
    Creature("Schizo", Attack(5), 22, "When summoned, opposite creature lose his abilities\nuntil schizo die.", reaction = new SchizoReaction),
    Creature("Ram", Attack(6), 26, "Opposite creature is destroyed and opponent get his mana back -2.", effects = effects(Direct -> ram)),
    Creature("Stranger", AttackSources().add(new StrangerAttack), 30, "Attack is highest opponent mana.\nWhen summoned, take effects of opposite slot.\n -immediate effects are not applied\n-can't duplicate effect to attack multiple targets", effects = effects(Direct -> merge)),
    Creature("Warp Queen", Attack(6), 32, "Opponent creatures lose their ability until end of next owner turn.\nDeals 4 damage to each of them", effects = effects(Direct -> warp))), eventListener = Some(OpponentListener(new WarpEventListener(_))))

  val photographer = Warp.cards(3)
  val stranger = Warp.cards(6)
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
      slot.add(SlotState(new MergeStranger(s.card , opp.card), s.life, s.status, s.card.attack, player.slots.getAttack(slot, s.card.attack), s.target, s.data))
    }
  }
  def photo : Effect = { env : Env =>
    import env._
    player.slots.filleds
      .find(s => s.get.card == photographer && s.num != selected && s.get.data != null)
      .foreach { s =>
        val backup = s.get.data.asInstanceOf[PlayerState.SlotsType]
        s.destroy()
        player.slots.slots.foreach{ s =>
          if (s.value.isEmpty){
            backup.get(s.num).foreach{ b =>
              s.add(SlotState(b.card, b.life, b.status, b.card.attack, player.slots.getAttack(s, b.card.attack), b.target, b.data))
            }
          }
        }
      }
    val slot = player.slots(selected)
    slot.setData(player.getSlots - selected)
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
    otherPlayer.slots.inflictCreatures(Damage(4, env, isAbility = true))
    player.addEffect(OnEndTurn -> new CountDown(2, { env : Env =>
      env.otherPlayer.slots.foreach(unbridle)
    }))
  }

  private val cache = collection.mutable.Map.empty[Card, MereMortal]

  def bridle(s : SlotState, slot : SlotUpdate){
    slot.write(Some(SlotState(cache.getOrElseUpdate(s.card, new MereMortal(s.card)), s.life, s.status, s.attackSources, slot.slots.getAttack(slot, s.attackSources), s.target, s.data)))
  }
  def unbridle(slot : SlotUpdate) {
    slot.value.foreach{ s =>
      s.card match {
        case m : MereMortal =>
          slot.remove()

          slot.add(SlotState(m.c, s.life, s.status, s.attackSources, slot.slots.getAttack(slot, m.c.attack), s.target, s.data))
        case _ =>
      }
    }
  }

  class SchizoReaction extends Reaction {

    override def onAdd(selected : SlotUpdate, slot : SlotUpdate) {
      if (selected.num == slot.num){
        val oppSlot = slot.slots.player.otherPlayer.slots(selected.num)
        oppSlot.value.foreach{ s =>
          oppSlot.remove()
          bridle(s, oppSlot)
        }
      }
    }
    override def onMyRemove(slot : SlotUpdate) {
      unbridle(slot.slots.player.otherPlayer.slots(slot.num)) // FIXME bugged between schizo and wq
    }
  }

  // code horror
  class WarpEventListener(inner : HouseEventListener) extends ProxyEventListener(inner) {
    private def isStranger(card : Card) = {
      card == stranger || card.isInstanceOf[MergeStranger]
    }
    override def refreshOnOppUpdate() {
      super.refreshOnOppUpdate()
      if (player.otherPlayer.housesUpdate.isDirty && player.getSlots.values.exists(s => isStranger(s.card))){
        player.slots.filleds.withFilter(s => isStranger(s.get.card)).foreach{ s =>
          s.attack.setDirty()
        }
      }
    }
  }
}

class ErrantAttack extends RunAttack {

  def apply(target : Option[Int], d : Damage, player : PlayerUpdate) {
    val num = target.get
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
      val card = cloaked.card
      slot.add(SlotState(card, cloaked.life, cloaked.status, card.attack, player.slots.getAttack(slot, card.attack), Some(slot.num), card.data))
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
  c.description,
  s.inputSpec, // don't care
  c.effects,
  c.mod,
  c.reaction,
  c.data,
  if (c.runAttack.isMultiTarget) s.runAttack else c.runAttack,
  c.isAltar,
  c.status){
  houseId = s.houseId
  houseIndex = s.houseIndex
  cost = s.cost
  id = c.id // !!!!
}
