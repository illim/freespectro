package priv.sp.house

import priv.sp._
import priv.sp.update._
import CardSpec._

trait Water {
  import GameCardEffect._

  val Water = House("water", List(
    Spell("Meditate", "Increase normal mana by 1", effects = effects(Direct -> addMana(1, 0, 2, 3))),
    Creature("seasprite", Attack(5), 22, "Every turn deals 2 damage to owner",
      effects = effects(OnTurn -> { env : Env =>
        env.focus()
        env.player.inflict(Damage(2)) })),
    Creature("MerfolkApostate", Attack(3), 10, "Give 2 fire mana when summoned", effects = effects(Direct -> focus(addMana(2, 0)))),
    Creature("IceGolem", Attack(4), 12, "Immune to spell & ability", immune = true),
    Creature("MerfolkElder", Attack(3), 16, "Increase air mana growth by 1", effects = effects(OnTurn -> addMana(1, 2))),
    Creature("IceGuard", Attack(3), 20, "Halve damage dealt to owner", mod = Some(new SpellProtectOwner(x => math.ceil(x / 2.0).intValue))),
    new GiantTurtle,
    Spell("AcidicShower", "Damage all creature by 15 and decrease mana of opponent by 1", effects = effects(Direct -> massDamage(Damage(15, isSpell = true)), Direct -> { env : Env =>
      env.otherPlayer.houses.incrMana(-1 , 0, 1, 2, 3, 4)
    })),
    Creature("SeaLord", Attack(7), 34, "Adjacent cards attack the turn they're summoned", reaction = new OverlordSlotReaction),
    Creature("WaterElemental", AttackSources().add(ManaAttack(1)), 38, "Heals owner by 10 when summoned", effects = effects(Direct -> focus(heal(10)), OnTurn -> addMana(1, 1))),
    Creature("MindMaster", Attack(6), 22, "Increase mana growth by 1", effects = effects(OnTurn -> addMana(1, 0, 1, 2, 3, 4))),
    Creature("AstralGuard", Attack(1), 17, "Decrease mana growth by 1", effects = effects(OnEndTurn -> { env : Env =>
      env.otherPlayer.houses.incrMana(-1 , 0, 1, 2, 3, 4)
    }))), houseIndex = 1)
}

class GiantTurtle extends Creature ("HugeTurtle", Attack(5), 17, "Absorb 5 damage"){
  override def inflict(damage : Damage, life : Int) = life - math.max(0, damage.amount - 5)
}

private class OverlordSlotReaction extends DefaultReaction {
  final override def onAdd(selected : Int, slot : SlotUpdate) = {
    if (math.abs(selected - slot.num) == 1){
      slot.toggle(runFlag)
    }
  }
}
