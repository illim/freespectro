package priv.sp.house

import priv.sp._
import priv.sp.update._

class Sower {
  import CardSpec._
  import GameCardEffect._

  val monsterPlant = Creature("MonsterPlant", Attack(6), 21, "when kills creature, heals completely all monster plants on the board.", runAttack = new MonsterPlantAttack)

  val Sower = House("Sower", List(
    Spell("Tangling", "Transfers X health from target creature to opposite creature\n(X = attack of target creature)",
          inputSpec = Some(SelectTargetCreature),
          effects = effects(Direct -> tangle)),
    Spell("Devouring", "Sacrifices friendly creature,\ndoubles attack of the rest of friendly creatures for 1 turn.",
          inputSpec = Some(SelectOwnerCreature),
          effects = effects(Direct -> devour)),
    monsterPlant,
    Spell("Pollination", "turns target creature of X level into special creature of (X minus 3) level\nwith full hp and heals X life to owner.",
          inputSpec = Some(SelectOwnerCreature),
          effects = effects(Direct -> pollinate)),
    Creature("BloodSundew", Attack(6), 24, "when deals damage, regenerates the same amount of hp.", runAttack = new BloodSundewAttack),
    Creature("PredatorPlant", Attack(6), 33, "when attacks creature, deals X additional damage to it\n(X = difference between its current and max hp).", runAttack = new PredatorPlantAttack),
    Creature("ForestDrake", Attack(5), 55, "when owner summons special creature,\ncreates its copy in nearest empty slot.", reaction = new ForestDrakeReaction),
    Creature("FieryFlower", Attack(0), 35, "Every turn halves health of enemy creature with highest hp and\n gives 1 fire power to owner.\nWhen enters the game, deals to opponent X damage (X = his fire power)", effects = effects(OnTurn -> fieryFlower, Direct -> { env : Env =>
      env.otherPlayer.inflict(Damage(env.player.getHouses(0).mana, env, isAbility = true))
    }))))

  Sower.initCards(Houses.basicCostFunc)

  private def tangle = { env : Env =>
    import env._
    val slot = otherPlayer.slots(selected)
    val damage = Damage(slot.get.attack, env, isSpell = true)
    slot.inflict(damage)
    val opp = player.slots(selected)
    if (opp.value.isDefined){
      opp.heal(damage.amount)
    }
  }

  private def devour = { env : Env =>
    import env._
    player.slots(selected).destroy()
    val factor = AttackFactor(2f)
    player.slots.foreach(_.attack.add(factor))
    player.addEffect(OnEndTurn -> new RemoveAttack(factor))
  }

  private def pollinate : Effect = { env : Env =>
    import env._
    val slot = player.slots(selected)
    val cost = slot.get.card.cost
    Sower.cards.find(_.cost == cost - 3).foreach{
      case c : Creature =>
        slot.destroy()
        player.slots.summon(selected, c)
      case _ =>
    }
    player.heal(cost)
  }

  private def fieryFlower = {env : Env =>
    import env._
    otherPlayer.slots.filleds.sortBy(_.get.life).lastOption foreach { slot =>
      updater.focus(selected, playerId)
      slot.inflict(Damage(math.ceil(slot.get.life / 2f).toInt, env, isAbility = true))
    }
    player.houses.incrMana(1, 0)
  }

  private class MonsterPlantAttack extends RunAttack {
    def apply(num : Int, d : Damage, player : PlayerUpdate) {
      val otherPlayer = player.otherPlayer
      val slot = otherPlayer.slots(num)
      if (slot.value.isEmpty) {
        otherPlayer.inflict(d)
      } else {
        slot.inflict(d)
        // FIXME maybe not good at all and should add source in damage?
        if (slot.value.isEmpty){
          player.slots.foreach{ slot =>
            if (slot.get.card == monsterPlant){
              slot.heal(monsterPlant.life)
            }
          }
        }
      }
    }
  }


  private class ForestDrakeReaction extends Reaction {

    final override def onSummon(selected : Int, selectedPlayerId : PlayerId, summoned : SummonEvent) {
      import summoned._
      if (selectedPlayerId == player.id && selected != num && card.houseId == Sower.houseId){
        player.updater.focus(selected, player.id)
        val slots = player.slots
        val dists = slotRange.collect{ case n if slots(n).value.isEmpty => (n, math.abs(n - selected)) }
        dists.sortBy(_._2).headOption.foreach{ case (pos, _) =>
          slots(pos).add(card)
        }
      }
    }
  }

}

// code horror
private class BloodSundewAttack extends RunAttack with DamageAttack {

  def apply(num : Int, d : Damage, player : PlayerUpdate) {
    val healAmount = damageAndGet(num, d, player)
    player.slots(num).heal(healAmount)
  }
}

private class PredatorPlantAttack extends RunAttack {

  def apply(num : Int, d : Damage, player : PlayerUpdate) {
    val otherPlayer = player.otherPlayer
    val slot = otherPlayer.slots(num)
    slot.value match {
      case None => otherPlayer.inflict(d)
      case Some(slotState) =>
        val x = slotState.card.life - slotState.life
        slot.inflict(d.copy(amount = d.amount + x))
    }
  }
}
