package priv.sp.house

import priv.sp._
import priv.sp.update._
import CardSpec._
import GameCardEffect._

class Dreamweaver {

  val Dreamweaver = House("Dreamweaver", List(
    Creature("Ethereal Wisp", AttackSources(Some(2), Vector(EtherealAttackSource)), 8, "Ethereal Wisp takes no damage from enemy spells and abilities.\nEthereal Wisp's attack is increased by 1 for each other owner's creature.", reaction = new EtherealReaction),
    Creature("Roc Hatchling", AttackSources(Some(6), Vector(RocAttackSource)), 17, "While Roc Hatchling is unopposed, its attack is halved (round up)."),
    Spell("Aurora", "All creatures are healed for 12 life and\nadd 1 mana of their own type to their owner's mana pools.", effects = effects(Direct -> aurora)),
    Creature("Spiritual Guide", Attack(4), 19, "When Spiritual Guide is summoned, it heals all owner's creatures\nare healed an amount equal to owner's Dream power,\nand heals its owner by 3 life for each owner's creature.", effects = effects(Direct -> guide)),
    Creature("Living Sword", AttackSources(Some(4), Vector(SwordAttackSource)), 28, "Living Sword gains 1 attack power for each neighboring creature.\nWhen Living Sword attacks, if there are creatures adjacent to\nthe opposing slot it also attacks those creatures.", reaction = new SwordReaction, runAttack = new SwordAttack),
    Creature("Rainbow Butterfly", Attack(4), 31, "Each time opponent summons a Creature,\nRainbow Butterfly's owner gains 3 mana of that creature's type.\n(All Special creatures count as Dream for this purpose.)", reaction = new RainbowReaction),
    Creature("Flying Castle", Attack(2), 37, "Flying Castle attacks opponent and all opponent's creatures.\nFlying Castle reduces damage done to all other owner's creatures by 2.", runAttack = MultiTargetAttack, reaction = new CastleReaction),
    Creature("Night Mare", AttackSources(Some(5), Vector(EtherealAttackSource)), 44, "When Nightmare is summoned, it deals 6 damage to each opponent's\ncreaturefor each empty opponent slot.\nNight Mare's attack is increased by 1 for each other owner's creature.", effects = effects(Direct -> mare), reaction = new NightmareReaction)), eventListener = Some(new CustomListener(new DreamweaverEventListener)))

  val roc = Dreamweaver.cards(1)
  val castle = Dreamweaver.cards(6)
  Dreamweaver.initCards(Houses.basicCostFunc)

  def aurora = { env : Env =>
    import env._
    def aur(p : PlayerUpdate) = p.slots.foreach{ s =>
      val card = s.get.card
      s.heal(12)
      p.houses.incrMana(1, card.houseIndex)
    }

    aur(player)
    aur(otherPlayer)
  }

  def guide = { env : Env =>
    import env._
    val nbCreatures = player.getSlots.size
    val dp = getMana(4)
    player.slots.foreach{ s =>
      if (s.num != selected) { // bs? apply effect before interception!?
        s.heal(dp)
      }
    }
    player.heal(nbCreatures * 3)
  }

  def mare = { env : Env =>
    import env._
    val damage = Damage(6 * otherPlayer.slots.slots.count(_.value.isEmpty), env, isAbility = true)
    otherPlayer.slots.inflictCreatures(damage)
  }

  class EtherealReaction extends NightmareReaction {
    override def selfProtect(d : Damage, slot : SlotUpdate) = {
      if (d.isEffect) d.copy(amount = 0) else d
    }
  }

  class NightmareReaction extends Reaction {
    override def onAdd(selected : SlotUpdate, slot : SlotUpdate) = {
      selected.attack.setDirty()
    }
    override def onDeath(selected : Int, playerId : PlayerId, dead : Dead) {
      import dead._
      player.slots(selected).attack.setDirty()
    }
  }

  private class SwordReaction extends Reaction {
    override def onAdd(selected : SlotUpdate, slot : SlotUpdate) = if (math.abs(selected.num - slot.num) == 1) selected.attack.setDirty()
    override def onDeath(selected : Int, playerId : PlayerId, dead : Dead) {
      if (math.abs(selected - dead.num) == 1) dead.player.slots(selected).attack.setDirty()
    }
  }

  private class RainbowReaction extends Reaction {

    override def onSummon(selected : Int, selectedPlayerId : PlayerId, summoned : SummonEvent) {
      import summoned._
      if (selectedPlayerId != player.id){
        val p = player.otherPlayer
        p.houses.incrMana(3, card.houseIndex)
      }
    }
  }

  class DreamweaverEventListener extends HouseEventListener {
    override def refreshOnOppUpdate() {
      super.refreshOnOppUpdate()
      if (player.otherPlayer.slotsUpdate.isDirty && player.getSlots.values.exists(_.card == roc)){
        player.slots.filleds.withFilter(_.get.card == roc).foreach{ s =>
          s.attack.setDirty()
        }
      }
    }
    override def protect(slot : SlotUpdate, damage : Damage) = {
      player.slots.foldl(damage) { (acc, s) =>
        val c = s.get.card
        if (c == castle){
          c.reaction.onProtect(s, DamageEvent(acc, Some(slot.num), player))
        } else acc
      }
    }
    override def onDeath(dead : Dead) {
      if (dead.player.id == player.id) {
        player.slots.foreach{ s =>
          if (s.num != dead.num){
            val c = s.get.card
            if (c.houseIndex == 4){
              c.reaction.onDeath(s.num, player.id, dead)
            }
          }
        }
      }
    }
  }
}

case object EtherealAttackSource extends AttackStateFunc {
  def apply(attack : Int, player : PlayerUpdate) = {
    val nbCreatures = player.slots.filleds.size
    attack + nbCreatures - 1
  }
}

case object RocAttackSource extends AttackSlotStateFunc {
  def apply(attack : Int, slot : SlotUpdate) = {
    if (slot.slots.player.otherPlayer.getSlots.isDefinedAt(slot.num)) {
      attack
    } else {
      attack / 2
    }
  }
}

case object SwordAttackSource extends AttackSlotStateFunc {
  def apply(attack : Int, slot : SlotUpdate) = {
    val nbAdjacents = slot.adjacentSlots.count(_.value.isDefined)
    attack + nbAdjacents
  }
}

class SwordAttack extends RunAttack {
  def apply(num : Int, d : Damage, player : PlayerUpdate) {
    val otherPlayer = player.otherPlayer
    val oppSlot     = otherPlayer.slots(num)
    val targets     = oppSlot :: oppSlot.adjacentSlots

    targets.foreach{ slot =>
      if (slot.num == num && slot.value.isEmpty) {
        player.otherPlayer.inflict(d)
      }

      if (slot.value.isDefined){
        slot.inflict(d)
      }
    }
  }
}

class CastleReaction extends Reaction {
  final override def onProtect(selected : SlotUpdate, d : DamageEvent) = {
    if (d.target.isDefined){
      d.damage.copy(amount = math.max(0, d.damage.amount - 2))
    } else d.damage
  }
}
