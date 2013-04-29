package priv.sp.house

import priv.sp._

trait Water {
  import CardSpec._
  import GameCardEffect._

  val Water = House("water", List(
    Spell("Meditate", "Increase normal mana by 1", effects = effects(Direct -> addMana(1, 0, 2, 3))),
    Creature("seasprite", Some(5), 22, "Every turn deals 2 damage to owner",
      effects = effects(OnTurn -> { env : Env =>
        env.focus()
        env.player.inflict(Damage(2)) })),
    Creature("MerfolkApostate", Some(3), 10, "Give 2 fire mana when summoned", effects = effects(Direct -> focus(addMana(2, 0)))),
    Creature("IceGolem", Some(4), 12, "Immune to spell & ability", immune = true),
    Creature("MerfolkElder", Some(3), 16, "Increase air mana growth by 1", effects = effects(OnTurn -> addMana(1, 2))),
    Creature("IceGuard", Some(3), 20, "Halve damage dealt to owner", mod = Some(new SpellProtectOwner(x => math.ceil(x / 2.0).intValue))),
    new GiantTurtle,
    Spell("AcidicRain", "Damage all creature by 15 and decrease mana of opponent by 1", effects = effects(Direct -> massDamage(Damage(15, isSpell = true)), Direct -> { env : Env =>
      env.otherPlayer.houses.incrMana(-1 , 0, 1, 2, 3, 4)
    })),
    Creature("MerfolkOverlord", Some(7), 34, "Adjacent cards attack the turn they're summoned", slotEffect = new OverlordSlotEffect),
    Creature("WaterElemental", None, 38, "Heals owner by 10 when summoned", effects = effects(Direct -> focus(heal(10)), OnTurn -> addMana(1, 1))),
    Creature("MindMaster", Some(6), 22, "Increase mana growth by 1", effects = effects(OnTurn -> addMana(1, 0, 1, 2, 3, 4))),
    Creature("AstralGuard", Some(1), 17, "Decrease mana growth by 1", effects = effects(OnEndTurn -> { env : Env =>
      env.otherPlayer.houses.incrMana(-1 , 0, 1, 2, 3, 4)
    }))), houseIndex = 1)
}

class GiantTurtle extends Creature ("GiantTurtle", Some(5), 17, "Absorb 5 damage"){
  override def inflict(damage : Damage, life : Int) = life - math.max(0, damage.amount - 5)
}

private class OverlordSlotEffect extends DefaultSlotEffect {
  final override def applySlot(selected : Int, num : Int, slot : SlotState) = {
    if (math.abs(selected - num) == 1){
      slot.copy(hasRunOnce = true)
    } else slot
  }
}
