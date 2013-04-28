package priv.sp.house

import priv.sp._
import GameCardEffect._
import CardSpec._

trait ZenMage {

  val Zen : House = House("Zen", List(
    Creature("Elementesist", Some(3), 11, "Deals damage to opposite card, and to all opposite card of same mana.", runAttack = new ElemAttack),
    Creature("RedlightBringer", Some(3), 15, "deals x additional damage to creatures on opposite and adjacent slots,\nwhere x is the number of owner adjacent creatures.", runAttack = new RedlightAttack),
    Spell("Focus", "Every owner card dedicate 50% of their attack to the focused creature.",
      inputSpec = Some(SelectTargetCreature),
      effects = effects(Direct -> focus)),
    Creature("ElectricGuard", Some(3), 20, "deals 3 damage to creatures damaging opponent."),
    Creature("Dreamer", Some(4), 19),
    Creature("Mimic", Some(4), 26),
    Creature("SpiralOfLight", Some(3), 27, "each turn, heals 1,2,3,2,1 to self and 4 adjacent cards\ndeals 1,2,3,2,1 to 5 opposite creatures", effects = effects(Direct -> spiral)),
    new ZenFighter))

  Zen.initCards(Houses.basicCostFunc)

  private class ElemAttack extends Attack {
    def apply(num : Int, d : Damage, updater : GameStateUpdater, id : PlayerId) {
      val otherPlayer = updater.players(other(id))
      otherPlayer.getSlots.get(num) match {
        case None => otherPlayer.inflict(d)
        case Some(oppositeSlot) =>
          val h = oppositeSlot.card.houseIndex
          otherPlayer.slots.value.foreach{ case (n, s) =>
            if (s.card.houseIndex == h){
              otherPlayer.slots.inflictCreature(n, d)
            }
          }
      }
    }
  }

  private class RedlightAttack extends Attack {
    def apply(num : Int, d : Damage, updater : GameStateUpdater, id : PlayerId) {
      val player = updater.players(id)
      val otherPlayer = player.otherPlayer

      val bonus = List(num -1, num+1).filter(player.slots.value.isDefinedAt _)
      val (targets, damage) = if (bonus.isEmpty) {
        (List(num), d)
      } else {
        ((num -1 to num +1), d.copy(amount = d.amount + bonus.size))
      }
      targets.foreach{ n =>
        otherPlayer.getSlots.get(n) match {
          case None => otherPlayer.inflict(d)
          case Some(oppositeSlot) => otherPlayer.slots.inflictCreature(n, d)
        }
      }
    }
  }

  private def focus = { env: Env =>
    import env._

    val damage = Damage(player.slots.value.toList.map(_._2.attack).sum / 2, isSpell = true)
    otherPlayer.slots.inflictCreature(env.selected, damage)
/**    ((PlayerState.emptySlots, List.empty[(Int, Int)]) /: player.slots.value){ case ((newslots, backup), (num, slot)) =>
      

    }*/
  }

  private def spiral = {env: Env =>
    import env._

    (selected-2 to selected +2).foreach{ num =>
      val amount = 3 - (selected - num)
      if (otherPlayer.slots.value.isDefinedAt(num)) otherPlayer.slots.inflictCreature(num, Damage(amount))
      if (player.slots.value.isDefinedAt(num)) player.slots.healCreature(num, amount)
    }
  }
}

class ZenFighter extends Creature ("ZenFighter", Some(7), 31, "When summoned gives 4 water mana. Zen Fighter receives 30% damage from spells and abilities", effects = effects(Direct -> focus(addMana(4, 1)))) {

  override def inflict(damage : Damage, life : Int) = {
    if (damage.isEffect) (life - math.ceil(0.3 * (damage.amount))).toInt
    else life - damage.amount
  }
}
