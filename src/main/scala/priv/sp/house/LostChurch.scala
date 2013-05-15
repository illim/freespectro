package priv.sp.house

import priv.sp._
import priv.sp.update._
/**
trait LostChurch {
  import CardSpec._
  import GameCardEffect._

  private val prisoner = Creature("Prisoner", Attack(3), 7, "When dying loose 1 mana of each basic houses.", reaction = new PrisonerReaction)
  private val enragedPrisoner = Creature("EnragedPrisoner", Attack(7), 45, "Immune to spell & ability when liberator alive.")

  val LostChurch = House("LostChurch", List(
    Spell("SpeedDrug", "Add +1 attack to owner creatures, deals to them 3 damage.",
          effects = effects(Direct -> speedDrug)),
    Creature("Preacher", Attack(5), 19, "When in play normal cards cost 1 more mana.\nIncrease growth of special mana by 1.", effects = effects(OnTurn -> addMana(1, 4))),
    Creature("FalseProphet", Attack(5), 19, "Give 2 mana to each basic house,\ntakes them back when dying.", effects = effects(Direct -> addMana(2, 0, 1, 2, 3))),
    Spell("WildJustice", "Deals (12 - attack) damage to each creature.",
          effects = effects(Direct -> wildJustice)),
    Creature("Scarecrow", Attack(8), 26, "Deals 10 damage on opposite creature, when dying heal opposite creature by 5.", effects = effects(Direct -> { env : Env =>
        env.focus()
        env.otherPlayer.slots.inflictCreature(env.selected, Damage(10, isAbility = true))
      })),
    new SkinnedBeast,
    Creature("Falconer", Attack(6), 28, "Each turns deals (2 * slot distance) damage to highest cost creature\n and creatures between.", effects = effects(OnTurn -> focus(falcon))),
    Creature("Liberator", Attack(4), 15, "Turns prisoner into Enraged prisoner. When dying inflict 15 damage to him.", reaction = new LiberatorReaction, effects = effects(Direct -> focus(deliverPrisoner)))),
    effects = List(OnEndTurn -> spawnPrisoner))

  LostChurch.initCards(Houses.basicCostFunc)

  private def spawnPrisoner : Effect = { env : Env =>
    import env._
    if (!player.slots.value.exists{ case (n, slot) => slot.card == prisoner || slot.card == enragedPrisoner }){
      val emptySlots = slotRange.filter(n => !player.slots.value.isDefinedAt(n))
      if (emptySlots.nonEmpty) {
        // todo deterministic random generator + exchange seed for multi
        player.slots.add(emptySlots(scala.util.Random.nextInt(emptySlots.size)), prisoner)
      }
    }
  }

  private def deliverPrisoner = { env : Env =>
    import env._
    player.slots.value.find{ case (n, slot) => slot.card == prisoner }.foreach{ case (n, slot) =>
      player.slots.destroy(n)
      player.slots.add(n, enragedPrisoner)
    }
  }

  private def speedDrug = { env : Env =>
    import env._
    player.slots.inflictCreatures(Damage(3, isSpell = true))
    player.slots.update(_.map{ case (n, slot) =>
      n -> slot.copy(attack = slot.attack + 1)
    })
  }

  private def wildJustice = { env : Env =>
    import env._
    player.slots.value.foreach{ case (n, slot) =>
      player.slots.inflictCreature(n, Damage(math.max(0, 12 - slot.attack), isSpell = true))
    }
    otherPlayer.slots.value.foreach{ case (n, slot) =>
      otherPlayer.slots.inflictCreature(n, Damage(math.max(0, 12 - slot.attack), isSpell = true))
    }
  }

  private def falcon = { env: Env =>
    import env._

    otherPlayer.slots.slots.toSeq.filter(_._1 != selected).sortBy(_._2.card.cost).lastOption foreach { case (num, slot) =>
      for(n <- selected to num by math.signum(num - selected)){
        otherPlayer.slots.inflictCreature(n, (Damage(2 * math.abs(n - selected), isAbility = true)))
      }
    }
  }

  class LiberatorReaction extends DefaultReaction {
    final override def onDeath(selected : Int, dead : Dead){
      import dead._
      if (selected == num){
        val player = updater.players(playerId)
        player.slots.value.find{ case (n, slot) => slot.card == enragedPrisoner }.foreach{ case (n, slot) =>
          player.slots.inflictCreature(n, Damage(15))
        }
      }
    }

    final override def onProtect(selected : Int, d : DamageEvent) = {
      import d._
      val player = updater.players(playerId)
      d.target match {
        case Some(n) if player.slots.value(n).card == enragedPrisoner && d.damage.isEffect =>
          d.damage.copy(amount = 0)
        case _ => d.damage
      }
    }
  }
}

class SkinnedBeast extends Creature("SkinnedBeast", Attack(8), 31, "Damage done to him is transformed into (10 - damage)."){
  override def inflict(damage : Damage, life : Int) = super.inflict(damage.copy(amount = math.max(0, 10 - damage.amount)), life)
}

class PrisonerReaction extends DefaultReaction {
  final override def onDeath(selected : Int, dead : Dead){
    import dead._
    if (selected == num){
      updater.players(playerId).houses.incrMana(-1, 0, 1, 2, 3)
    }
  }
}

class FalseProphetReaction extends DefaultReaction {
  final override def onDeath(selected : Int, dead : Dead){
    import dead._
    if (selected == num){
      updater.players(playerId).houses.incrMana(-2, 0, 1, 2, 3)
    }
  }
}
*/
