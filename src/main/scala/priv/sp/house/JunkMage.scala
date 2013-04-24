package priv.sp.house

import priv.sp._

trait JunkMage {
  import CardSpec._
  import GameCardEffect._

  val Junk = House("Junk", List(
    Creature("Screamer", Some(2), 13, "+1 attack for each screamer in play", slotEffect = new ScreamerSlotEffect),
    Spell("PoisonFlower", "Deals 5 damage to target and creatures around. Deals 3 damage to opponent.",
          inputSpec = Some(SelectTargetCreature),
          effects = effects(Direct -> poisonFlower)),
    Creature("ChainController", Some(4), 21, "When adjacent creature of cost <6 die, fill the slot with another weak creature nearby"),
    Creature("JunkyardGoddess", Some(4), 26, "Absorb 3 of first damage done to either owner or creature of cost < 6"),
    Creature("RoamingAssassin", Some(6), 27, "If unblocked, move to the closest next unblocked opponent and deals 5 damage to it"),
    Creature("Factory", Some(4), 29, "When spawning a card of cost < 6 onto it, it produce 2 creatures in its adjacent slots, and deals 3 damage to owner"),
    Creature("RecyclingBot", Some(8), 29, "Every turn deals 8 damage to opponent creature with most life"),
    Creature("TrashCyborg", Some(3), 30, "Fill the board with trash 2/11 and one cyborg. Every turn 2 pieces of trash assemble into the cyborg")))

  Junk.initCards(Houses.basicCostFunc)

  private val screamer = Junk.cards(0)

  private def poisonFlower = { env: Env =>
    import env._

    val damage = Damage(5, isSpell = true)
    (selected -1 to selected +1).foreach{ num =>
      if (otherPlayer.slots.value.isDefinedAt(num)) otherPlayer.slots.inflictCreature(num, damage)
      if (player.slots.value.isDefinedAt(num)) player.slots.inflictCreature(num, damage)
    }
    otherPlayer.inflict(Damage(3, isSpell = true))
  }

  private class ScreamerSlotEffect extends DefaultSlotEffect {
    final override def applySlots(selected : Int, slots : PlayerState.SlotsType) = {
      applyAttackBonus(selected, slots, 1)
    }

    final override def unapplySlots(selected : Int, slots : PlayerState.SlotsType) = {
      applyAttackBonus(selected, slots, -1)
    }

    private def applyAttackBonus(selected : Int, slots : PlayerState.SlotsType, fact : Int = 1) = {
      val nbScreamers = slots.count(_._2 .card == screamer)
      (slots /: slots){ case (acc, (n, slot)) =>
        acc + (n -> (if (slot.card == screamer){
          if (n == selected) {
            slot.copy(attack = slot.attack + fact * nbScreamers)
          } else {
            slot.copy(attack = slot.attack + fact * 1)
          }
        } else slot))
      }
    }
  }
}
