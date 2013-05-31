package priv.sp.house

import priv.sp._

class Trooper {
  import CardSpec._
  import GameCardEffect._

  val Trooper = House("Troopers", List(
    Spell("Conscription", "Increase troopers mana by 1", effects = effects(Direct -> addMana(1, 4))),
    Creature("Marine", Attack(4), 17, "Deals 4 damage to summoned opponent creatures", reaction = new MarineReaction),
    Creature("Barracks", Attack(2), 17, "Increase troopers mana growth by 1", effects = effects(OnTurn -> addMana(1, 4))),
    Creature("Wraith", Attack(4), 24, "Every turn deals 2 damage to opponent creatures", effects = effects(OnTurn -> focus(damageCreatures(2, isAbility = true)))),
    new Goliath,
    Creature("SiegeTank", Attack(8), 29, "Every turn deals 8 damage to opponent creature with most life", effects = effects(OnTurn -> focus(siege))),
    Spell("NuclearMissile", "Deals 19 damage to opponent creatures", effects = effects(Direct -> damageCreatures(19, isSpell = true))),
    Creature("ScienceVessel", Attack(6), 60, "When summoned deals 12 damage to opponent creatures", effects = effects(Direct -> damageCreatures(12, isAbility = true)))))

  val marine = Trooper.cards(1)
  Trooper.initCards({ i: Int => if (i == 0) i else i + 1 })

  private def siege = { env: Env =>
    import env._

    otherPlayer.slots.filleds.sortBy(_.get.life).lastOption foreach { slot =>
      slot.inflict(Damage(8, env, isAbility = true))
    }
  }

  class MarineReaction extends Reaction {

      final override def onSummon(selected : Int, selectedPlayerId : PlayerId, summoned : SummonEvent) {
        import summoned._
        if (selectedPlayerId != player.id){
          val damage = Damage(4, Context(selectedPlayerId, Some(marine), selected), isAbility = true)
          player.updater.focus(selected, selectedPlayerId)
          player.slots(num).inflict(damage)
        }
      }
  }
}

class Goliath extends Creature("Goliath", Attack(6), 20, "Immune to spell and absorb 1 damage", immune = true){
  override def inflict(damage : Damage, life : Int) = if (damage.isEffect) life else (life - math.max(0, damage.amount - 1))
}
