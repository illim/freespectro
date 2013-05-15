package priv.sp.house

import priv.sp._
import priv.sp.update._

trait Fire {
  import CardSpec._
  import GameCardEffect._

  val Fire = House("fire", List(
    Creature("Goblin", Attack(4), 16, "Every turn deals 2 damage to owner adjacent cards", effects = effects(OnTurn -> goblinBerserker)),
    Creature("WallofFlame", Attack(0), 5, "Deals 5 damage to opponent creatures when summoned", effects = effects(Direct -> damageCreatures(Damage(5, isAbility = true)))),
    Creature("FireMonk", Attack(3), 13, "Every turn increase fire mana growth by 1", effects = effects(OnTurn -> addMana(1, 0))),
    Creature("Drake", Attack(4), 18, "Attack the turn he is summoned", status = runFlag),
    Creature("OrcChieftain", Attack(3), 16, "Increase attack of adjacent card by 2", reaction = new OrcSlotReaction),
    Spell("FlameWave", "Deals 9 damage to opponent creatures", effects = effects(Direct -> damageCreatures(Damage(9, isSpell = true)))),
    Creature("BullCommander", Attack(6), 20, "Increase attack of owner card by 1", reaction = new BullSlotReaction),
    Creature("Blargl", Attack(8), 25, "Deals 4 damage to every creature when summoned", effects = effects(Direct -> massDamage(Damage(4, isAbility = true), immuneSelf = true))),
    Spell("Inferno", "Deals 18 damage to target and 10 to other opponent creatures", inputSpec = Some(SelectTargetCreature), effects = effects(Direct -> inferno)),
    Creature("FireElemental", AttackSources().add(ManaAttack(0)), 36, "Fire Elemental deals 3 damage to opponent creatures when summoned", effects = effects(Direct -> damageCreatures(Damage(3, isAbility = true)), Direct -> focus(damage(Damage(3, isAbility = true))), OnTurn -> addMana(1, 0))),
    Spell("Apocalypse", "Damage any creature and opponent by 8 + fire mana", effects = effects(Direct -> armageddon)),
    Creature("Dragon", Attack(9), 41, "Increase spell damage by 50%", mod = Some(new SpellMod(x => math.ceil(x * 1.5).intValue))))
    , houseIndex = 0)

  private def goblinBerserker = { env: Env =>
    val damage = Damage(2, isAbility = true)
    val targets = env.player.slots(env.selected).adjacentSlots.filter(_.value.isDefined)
    if (targets.nonEmpty) {
      env.focus()
      targets.foreach(_.inflict(damage))
    }
  }

  private def inferno = { env: Env =>
    import env._

    val damage = Damage(10, isSpell = true)
    otherPlayer.slots.foreach{ slot =>
      slot.inflict(
        if (slot.num == selected) Damage(18, true) else damage)
    }
  }


  private def armageddon = { env: Env =>
    import env._

    val d = Damage(getMana(0) + 8, isSpell = true)
    env.otherPlayer.inflict(d)
    massDamage(d)(env)
  }
}

case object OrcAttackBonus extends AttackFunc { def apply(attack : Int) = attack + 2 }
case object BullAttackBonus extends AttackFunc { def apply(attack : Int) : Int = attack + 1 }

private class OrcSlotReaction extends AttackBonusReaction {
  final def cond(selected : Int, num : Int) = math.abs(selected - num) == 1
    val bonus = OrcAttackBonus
}

private class BullSlotReaction extends AttackBonusReaction {
  final def cond(selected : Int, num : Int) = selected != num
  val bonus = BullAttackBonus
}

private abstract class AttackBonusReaction extends DefaultReaction {
  def cond(selected : Int, num : Int) : Boolean
  val bonus : AttackSource

  final override def onAdd(selected : Int, slot : SlotUpdate) = {
    if (selected == slot.num){
      slot.slots.foreach{ s =>
        if (cond(s.num, slot.num)) s.attack.add(bonus)
      }
    } else if (cond(selected, slot.num)) {
      slot.attack.add(bonus)
    }
  }

  final override def onRemove(slot : SlotUpdate) = {
    slot.slots.foreach{ s =>
      if (cond(s.num, slot.num)) {
        s.attack.remove(bonus)
      }
    }
  }
}
