package priv.sp.house

import priv.sp._

trait Fire {
  import CardSpec._
  import GameCardEffect._

  val Fire = House("fire", List(
    Creature("Goblin", Some(4), 16, "Every turn deals 2 damage to owner adjacent cards", effects = effects(OnTurn ->goblinBerserker)),
    Creature("WallofFire", Some(0), 5, "Deals 5 damage to opponent creatures when summoned", effects = effects(Direct -> damageCreatures(Damage(5, isAbility = true)))),
    Creature("FireMonk", Some(3), 13, "Every turn increase fire mana growth by 1", effects = effects(OnTurn -> addMana(1, 0))),
    Creature("FireDrake", Some(4), 18, "Attack the turn he is summoned", runOnce = true),
    Creature("OrcChieftain", Some(3), 16, "Increase attack of adjacent card by 2", slotEffect = new OrcSlotEffect),
    Spell("FlameWave", "Deals 9 damage to opponent creatures", effects = effects(Direct -> damageCreatures(Damage(9, isSpell = true)))),
    Creature("BullCommander", Some(6), 20, "Increase attack of owner card by 1", slotEffect = new BullSlotEffect),
    Creature("Bargul", Some(8), 25, "Deals 4 damage to every creature when summoned", effects = effects(Direct -> massDamage(Damage(4, isAbility = true)))),
    Spell("Inferno", "Deals 18 damage to target and 10 to other opponent creatures", inputSpec = Some(SelectTargetCreature), effects = effects(Direct -> inferno)),
    Creature("FireElemental", None, 36, "Fire Elemental deals 3 damage to opponent creatures when summoned", effects = effects(Direct -> damageCreatures(Damage(3, isAbility = true)), OnTurn -> addMana(1, 0))),
    Spell("Armageddon", "Damage any creature and opponent by 8 + fire mana", effects = effects(Direct -> armageddon)),
    Creature("Dragon", Some(9), 41, "Increase spell damage by 50%", mod = Some(new SpellMod(x => math.ceil(x * 1.5).intValue))))
    , houseIndex = 0)

  private def goblinBerserker = { env: Env =>
    val damage = Damage(2, isAbility = true)
    env.player.slots.slots.foreach { case (num, slot) =>
      if (math.abs(num - env.selected) == 1) {
        env.player.slots.inflictCreature(num, damage)
      }
    }
  }

  private def inferno = { env: Env =>
    import env._

    val damage = Damage(10, isSpell = true)
    otherPlayer.slots.slots.foreach{ case (num, slot) => // suck
      otherPlayer.slots.inflictCreature(num,
        if (num == selected) Damage(18, true) else damage)
    }
  }


  private def armageddon = { env: Env =>
    import env._

    val d = Damage(getMana(0) + 8, isSpell = true)
    env.otherPlayer.inflict(d)
    massDamage(d)(env)
  }
}

private class OrcSlotEffect extends AttackBonusSlotEffect {
  final def cond(selected : Int, num : Int) = math.abs(selected - num) == 1
  val amount = 2
}

private class BullSlotEffect extends AttackBonusSlotEffect {
  final def cond(selected : Int, num : Int) = selected != num
  val amount = 1
}

private abstract class AttackBonusSlotEffect extends DefaultSlotEffect {
  private val some0 = Some(0)
  def cond(selected : Int, num : Int) : Boolean
  def amount : Int

  final override def applySlot(selected : Int, num : Int, slot : SlotState) = {
    if (cond(selected, num) && slot.card.attack != some0){
      slot.copy(attack = slot.attack + amount)
    } else slot
  }

  final override def applySlots(selected : Int, slots : PlayerState.SlotsType) = {
    applyAttackBonus(selected, slots, 1)
  }

  final override def unapplySlots(selected : Int, slots : PlayerState.SlotsType) = {
    applyAttackBonus(selected, slots, -1)
  }

  private def applyAttackBonus(selected : Int, slots : PlayerState.SlotsType, fact : Int = 1) = {
    (slots /: slots){ case (acc, (n, slot)) =>
      acc + (n -> (if (cond(selected, n) && slot.card.attack != some0){
        slot.copy(attack = slot.attack + fact * amount)
      } else slot))
    }
  }
}
