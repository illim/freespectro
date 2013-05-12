package priv.sp.house

import priv.sp._

trait Mecanic {
  import CardSpec._
  import GameCardEffect._


  val Mecanic = House("Mechanics", List(
    Spell("Overtime", "Increase mechanic mana by 1", effects = effects(Direct -> addMana(1, 4))),
    Creature("DwarvenRifleman", Some(4), 17, "Deals 4 damage to summoned opponent creatures", reaction = new DwarfRiflemanReaction),
    Creature("DwarvenCraftsman", Some(2), 17, "Increase mechanic mana growth by 1", effects = effects(OnTurn -> addMana(1, 4))),
    Creature("Ornithopter", Some(4), 24, "Every turn deals 2 damage to opponent creatures", effects = effects(OnTurn -> focus(damageCreatures(Damage(2, isAbility = true))))),
    new SteelGolem,
    Creature("Cannon", Some(8), 29, "Every turn deals 8 damage to opponent creature with most life", effects = effects(OnTurn -> focus(cannon))),
    Spell("Cannonade", "Deals 19 damage to opponent creatures", effects = effects(Direct -> damageCreatures(Damage(19, isSpell = true)))),
    Creature("SteamTank", Some(6), 60, "When summoned deals 12 damage to opponent creatures", effects = effects(Direct -> damageCreatures(Damage(12, isAbility = true))))))

  Mecanic.initCards({ i: Int => if (i == 0) i else i + 1 })


  private def cannon = { env: Env =>
    import env._

    otherPlayer.slots.slots.toSeq.sortBy(_._2.life).lastOption foreach { case (num, slot) =>
      otherPlayer.slots.inflictCreature(num, (Damage(8, isAbility = true)))
    }
  }
}


class DwarfRiflemanReaction extends DefaultReaction {
  val damage = Damage(4, isAbility = true)
  final override def onSummon(selected : Int, selectedPlayerId : PlayerId, summoned : SummonEvent) {
    import summoned._
    if (selectedPlayerId != playerId){
      updater.focus(selected, selectedPlayerId)
      updater.players(playerId).slots.inflictCreature(num, damage)
    }
  }
}

class SteelGolem extends Creature("SteelGolem", Some(6), 20, "Immune to spell and absorb 1 damage", immune = true){
  override def inflict(damage : Damage, life : Int) = if (damage.isEffect) life else (life - math.max(0, damage.amount - 1))
}
