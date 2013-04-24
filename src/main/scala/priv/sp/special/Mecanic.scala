package priv.sp.special

import priv.sp._

trait Mecanic {
  import CardSpec._
  import GameCardEffect._


  val Mecanic = House("Mechanics", List(
    Spell("Overtime", "Increase mechanic mana by 1", spec = spell(Direct -> addMana(1, 4))),
    Creature("DwarvenRifleman", Some(4), 17, "Deals 4 damage to summoned opponent creatures", boardEffect = Some(InterceptSpawn(Damage(4, isAbility = true)))),
    Creature("DwarvenCraftsman", Some(2), 17, "Increase mechanic mana growth by 1", spec = creature(OnTurn -> addMana(1, 4)), isFocusable = false),
    Creature("Ornithopter", Some(4), 24, "Every turn deals 2 damage to opponent creatures", spec = creature(OnTurn -> damageCreatures(Damage(2, isAbility = true)))),
    new SteelGolem,
    Creature("Cannon", Some(8), 29, "Every turn deals 8 damage to opponent creature with most life", spec=creature(OnTurn -> cannon)),
    Spell("Cannonade", "Deals 19 damage to opponent creatures", spec = spell(Direct -> damageCreatures(Damage(19, isSpell = true)))),
    Creature("SteamTank", Some(6), 60, "When summoned deals 12 damage to opponent creatures", spec = creature(Direct -> damageCreatures(Damage(12, isAbility = true))))))

  Mecanic.initCards({ i: Int => if (i == 0) i else i + 1 })


  private def cannon = { env: Env =>
    import env._

    otherPlayer.slots.slots.toSeq.sortBy(_._2.life)(math.Ordering.Int.reverse).headOption foreach { case (num, slot) =>
      otherPlayer.slots.inflictCreature(num, (Damage(8, isAbility = true)))
    }
  }
}
