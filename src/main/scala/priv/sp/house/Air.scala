package priv.sp.house

import priv.sp._

trait Air {
  import CardSpec._
  import GameCardEffect._

  val Air = House("air", List(
    Creature("FaerieNovice", Attack(4), 11, "Increase spell damage by 1", mod = Some(new SpellMod(x => x + 1))),
    Creature("Griffin", Attack(3), 15, "If Air mana >= 5 deals 5 damage to opponent when summoned",
      effects = effects(Direct -> { env : Env =>
        if (env.getMana(2) > 4) env.otherPlayer.inflict(Damage(5))
      })),
    Spell("CalltoThunder", "Deals 6 damage to target creature and opponent",
      inputSpec = Some(SelectTargetCreature),
      effects = effects(Direct -> damageCreature(Damage(6, isSpell = true)), Direct -> damage(Damage(6, isSpell = true)))),
    Creature("AirMedic", Attack(4), 19, "When summoned heals owner by amount of earth mana(limited to 10)",
      effects = effects(Direct -> { env : Env =>
        env.focus()
        env.player.heal(math.min(env.getMana(3), 10))
      })),
    Creature("LightningFence", Attack(0), 28, "Every turn deals 4 damage to opponent", effects = effects(OnTurn -> focus(damage(Damage(4, isAbility = true))))),
    Spell("Lightnin", "Deals (5 + air power) damage to opponent",
      effects = effects(Direct -> { env : Env => env.otherPlayer.inflict(Damage(5 + env.getMana(2), isSpell = true))})),
    Creature("Phoenix", Attack(6), 18, "Can reborn if fire mana >= 10", reaction = new PhoenixReaction),
    Spell("ChainLightning", "Deals 9 damage to opponent and his creatures", effects = effects(Direct -> damageCreatures(Damage(9, isSpell = true)), Direct -> damage(Damage(9, isSpell = true)))),
    Creature("Cloud", Attack(4), 20, "Attack all opponent creatures", runAttack = MultiTargetAttack),
    Spell("Twister", "destroy target",
      inputSpec = Some(SelectTargetCreature),
      effects = effects(Direct -> { env: Env => env.otherPlayer.slots(env.selected).destroy() })),
    Creature("AirElemental", AttackSources().add(ManaAttack(2)), 44, "Air elemental deals 8 damage to opponent when summoned", effects = effects(Direct -> focus(damage(Damage(8, isAbility = true))), OnTurn -> addMana(1, 2))),
    Creature("Titan", Attack(9), 40, "Deals 15 damage to opposite creature when summoned",
      effects = effects(Direct -> { env : Env =>
        env.focus()
        env.otherPlayer.slots(env.selected).inflict(Damage(15, isAbility = true))
      })))
      , houseIndex = 2)

}

class PhoenixReaction extends DefaultReaction {
  final override def onDeath(selected : Int, dead : Dead){
    import dead._
    if (selected == num){
      val playerUpdate = updater.players(playerId)
      if (playerUpdate.houses.value(0).mana > 9) {
        playerUpdate.slots(num).add(dead.card)
      }
    }
  }
}
