package priv.sp.house

import priv.sp._
import priv.sp.update._

/**
 * done :
 * noctule
 * bloodTies
 */

class Vampire {
  import CardSpec._
  import GameCardEffect._

  val Vampire : House = House("Vampires", List(
    Spell("DarkFlock", "Moves owner's creature with lowest hp in target slot\nand makes it invulnerable for 1 turn if its level is not higher than 9.",
          inputSpec = Some(SelectOwnerSlot),
          effects = effects(Direct -> darkFlock)),
    Creature("Noctule", Attack(4), 17, "When deals damage to opponent, heals owner the same amount of life.", runAttack = new NoctuleAttack),
    Creature("Ghoul", Attack(4), 17, "If opposite creature has less than 11 life, kills it at one blow.", runAttack = new GhoulAttack),
    Creature("Acolyte", Attack(4), 17, "When enemy creature receives more than 8 damage,\ngives owner 1 mana of the same element."),
    Spell("BloodTies", "destroys owner's creature and permanently increases attack of\nits neighbors by its attack\n(doesn't affect creatures with mass attack and creatures of level > 9)",
          inputSpec = Some(SelectOwnerCreature),
          effects = effects(Direct -> bloodTies)),
    Creature("Nosferatu", Attack(5), 34, "When creature dies, heals owner by 3 and himself by 2."),
    Creature("Aristocrat", Attack(5), 34, "At the beginning of turn moves in slot opposite to opponent's creature with\n lowest hp(can switch places with friendly creature).\nWhen kills creature deals opponent damage equal to its attack.", runAttack = new AristoAttack),
    Creature("Mansion", Attack(0), 40, "When owner's non-special creature dies, replaces it with neophyte 5/14.\nOn entering the game turns its neighbors into ghouls.", effects = effects(Direct -> ghoulify))))

  val ghoul = Vampire.cards(2).asCreature
  Vampire.initCards(Houses.basicCostFunc)

  val neophyte = Creature("Neophyte", Attack(5), 14, "Heals himself half of damage dealt to enemies.")

  private def lowestLife(s1 : SlotUpdate, s2 : SlotUpdate) = if (s2.get.life < s1.get.life) s2 else s1

  private def darkFlock = { env: Env =>
    import env._
    player.slots.reduce(lowestLife).foreach{ s =>
      val card = s.get.card
      player.slots.move(s.num, selected)
      if (card.cost < 10){
        // TODO
      }
    }
  }

  private def bloodTies = { env: Env =>
    import env._
    val slot = player.slots(selected)
    val attack = slot.get.attack
    slot.destroy()
    player.slots(selected).adjacentSlots.foreach{ s =>
      if (s.value.isDefined){
        val card = s.get.card
        if (card.cost < 9 && card.runAttack != MultiTargetAttack) {
          val bonus = AttackAdd(attack)
          s.attack.add(bonus)
        }
      }
    }
  }

  private def aristo = { env: Env =>
    import env._
    val otherSlots = otherPlayer.slots
    val slots      = player.slots
    otherPlayer.slots.reduce(lowestLife).foreach{ s =>
      if (s.num != selected){
        // todo switch
//        slots.move(env.selected, dest.num)
      }
    }
  }

  private def ghoulify : Effect = { env: Env =>
    import env._
    player.slots(selected).adjacentSlots.foreach{ s =>
      if (s.value.isDefined){
        s.destroy()
        s.add(ghoul)
      }
    }
  }

}

private class NoctuleAttack extends RunAttack {
  def apply(num : Int, d : Damage, player : PlayerUpdate) {
    val otherPlayer = player.otherPlayer
    val slot = otherPlayer.slots(num)
    slot.value match {
      case None =>
        val oldl = otherPlayer.value.life
        otherPlayer.inflict(d, Some(SlotSource(player.id, num)))
        player.heal(oldl - otherPlayer.value.life)
      case Some(_) =>
        slot.inflict(d)
    }
  }
}

private class GhoulAttack extends RunAttack {
  def apply(num : Int, d : Damage, player : PlayerUpdate) {
    val otherPlayer = player.otherPlayer
    val slot = otherPlayer.slots(num)
    slot.value match {
      case None =>
        otherPlayer.inflict(d, Some(SlotSource(player.id, num)))
      case Some(slotState) =>
        if (slotState.life < 11){
          slot.destroy
        } else slot.inflict(d)
    }
  }
}

// TODO broadcast death
class NosferatuReaction extends DefaultReaction {
  final override def onDeath(selected : Int, dead : Dead){
    import dead._
    player.slots(selected).heal(2)
    player.heal(3)
  }
}

class AristoAttack extends RunAttack {
  def apply(num : Int, d : Damage, player : PlayerUpdate) {
    val otherPlayer = player.otherPlayer
    val slot = otherPlayer.slots(num)
    slot.value match {
      case None =>
        otherPlayer.inflict(d, Some(SlotSource(player.id, num)))
      case Some(_) =>
        val attack = slot.get.attack
        slot.inflict(d)
        if (slot.value.isEmpty) {
          otherPlayer.inflict(Damage(attack, isAbility = true), Some(SlotSource(player.id, num)))
        }
    }
  }
}
