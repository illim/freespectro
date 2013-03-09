package priv.sp

case class House(name: String, cards: List[Card])

class Houses extends HouseCardEffects {
  import CardSpec._
  import GameCardEffect._

  val Fire = House("fire", List(
    Creature("GoblinBerserker", Some(4), 16, spec = creature(OnTurn ->goblinBerserker)),
    Creature("WallofFire", Some(0), 5, spec = creature(Direct -> damageCreatures(Damage(5, isAbility = true)))),
    Creature("PriestOfFire", Some(3), 13, spec = creature(OnTurn -> addMana(1, 0))),
    Creature("FireDrake", Some(4), 18, spec = creature(Direct -> toggleRun)),
    Creature("OrcChieftain", Some(3), 16),
    Spell("FlameWave", spec = spell(Direct -> damageCreatures(Damage(9, isSpell = true)))),
    Creature("MinotaurCommander", Some(6), 20),
    Creature("Bargul", Some(8), 25, spec = creature(Direct -> bargul)),
    Spell("Inferno", inputSpec = Some(SelectTargetCreature), spec = spell(Direct -> inferno)),
    Creature("FireElemental", None, 36, spec = creature(OnTurn -> addMana(1, 0))),
    Spell("Armageddon"),
    Creature("Dragon", Some(9), 41, mod = Some(new SpellMod(x => math.ceil(x * 1.5).intValue)))))

  val Water = House("water", List(
    Spell("Meditation", spec = spell(Direct -> addMana(1, 0, 2, 3))),
    Creature("SeaSprite", Some(5), 22,
      spec = creature(OnTurn -> { env : Env => env.player.life.%==( _ - 2) })),
    Creature("MerfolkApostate", Some(3), 10, spec = creature(Direct -> addMana(2, 0))),
    Creature("IceGolem", Some(4), 12, immune = true),
    Creature("MerfolkElder", Some(3), 16, spec = creature(OnTurn -> addMana(1, 2))),
    Creature("IceGuard", Some(3), 20, mod = Some(new SpellProtectOwner(x => math.ceil(x / 2.0).intValue))),
    new Creature("GiantTurtle", Some(5), 17){
      override def inflict(damage : Damage, life : Int) = life - math.max(0, damage.amount - 5)
    },
    Spell("AcidicRain", spec = spell(Direct -> massDamage(Damage(15, isSpell = true)), Direct -> { env : Env =>
      env.otherPlayer.houses.%=={ houses => HouseState.incrMana(houses, -1 , 0, 1, 2, 3, 4) }
    })),
    Creature("MerfolkOverlord", Some(7), 34),
    Creature("WaterElemental", None, 38, spec = creature(Direct -> heal(10), OnTurn -> addMana(1, 1))),
    Creature("MindMaster", Some(6), 22, spec = creature(OnTurn -> addMana(1, 0, 1, 2, 3, 4))),
    Creature("AstralGuard", Some(1), 17, spec = creature(OnTurn -> { env : Env =>
      env.otherPlayer.houses.%=={ houses => HouseState.incrMana(houses, -1 , 0, 1, 2, 3, 4) }
    }))))

  val Air = House("air", List(
    Creature("FaerieApprentice", Some(4), 11, mod = Some(new SpellMod(x => x + 1))),
    Creature("Griffin", Some(3), 15,
      spec = creature(Direct -> { env : Env =>
        if (env.getMana(2) > 4) env.otherPlayer.life.%==( _ - 5) else GameState.unit
      })),
    Spell("CalltoThunder",
      inputSpec = Some(SelectTargetCreature),
      spec = spell(Direct -> damageCreature(Damage(6, isSpell = true)), Direct -> damage(Damage(6, isSpell = true)))),
    Creature("FaerieSage", Some(4), 19,
      spec = creature(Direct -> { env : Env =>
        env.player.life.%==( _ + math.min(env.getMana(3), 10))
      })),
    Creature("WallOfLightning", Some(0), 28, spec = creature(OnTurn -> damage(Damage(4, isAbility = true)))),
    Spell("LightningBolt",
      spec = spell(Direct -> { env : Env => env.otherPlayer.life.%==( _ - 5 - env.getMana(2))})),
    Creature("Phoenix", Some(6), 18),
    Spell("ChainLightning", spec = spell(Direct -> damageCreatures(Damage(9, isSpell = true)), Direct -> damage(Damage(9, isSpell = true)))),
    Creature("LightningCloud", Some(4), 20, multipleTarget = true),
    Spell("Tornado",
      inputSpec = Some(SelectTargetCreature),
      spec = spell(Direct -> { env: Env => env.otherPlayer.slots.%==( _ - env.selected) })),
    Creature("AirElemental", None, 44, spec = creature(Direct -> damage(Damage(8, isAbility = true)), OnTurn -> addMana(1, 2))),
    Creature("Titan", Some(9), 40,
      spec = creature(Direct -> { env : Env => SlotState.inflictCreature(env.otherPlayer, env.selected, Damage(15, isAbility = true))}))))

  val Earth = House("earth", List(
    Creature("ElvenHealer", Some(2), 12,
      spec = creature(OnTurn -> heal(2))),
    Spell("NaturesRitual",
      inputSpec = Some(SelectOwnerCreature),
      spec = spell(Direct -> heal(8), Direct -> healCreature(8))),
    Creature("ForestSprite", Some(1), 22, multipleTarget = true),
    Spell("Rejuvenation",
      spec = spell(Direct -> { env : Env => env.player.life.%==( _ + 2 * env.getMana(3))})),
    Creature("elfHermit", Some(1), 13, spec = creature(OnTurn -> addMana(2, 3))),
    Spell("NaturesFury"),
    Creature("GiantSpider", Some(4), 21),
    Creature("Troll", Some(6), 25, spec = creature(OnTurn -> healCreature(4))),
    Spell("StoneRain", spec = spell(Direct -> massDamage(Damage(25, isSpell = true)))),
    Creature("EarthElemental", None, 49, spec = creature(OnTurn -> addMana(1, 2))),
    Creature("MasterHealer", Some(3), 35,
      spec = creature(OnTurn -> heal(3), OnTurn -> healCreatures(3))),
    Creature("Hydra", Some(3), 40,
      multipleTarget = true,
      spec = creature(OnTurn -> healCreature(4)))))

  val Mecanic = House("Mechanics", List(
    Spell("Overtime", spec = spell(Direct -> addMana(1, 4))),
    Creature("DwarvenRifleman", Some(4), 17),
    Creature("DwarvenCraftsman", Some(2), 17, spec = creature(OnTurn -> addMana(1, 4))),
    Creature("Ornithopter", Some(4), 24, spec = creature(OnTurn -> damageCreatures(Damage(2, isAbility = true)))),
    Creature("SteelGolem", Some(6), 20, immune = true),
    Creature("Cannon", Some(8), 29),
    Spell("Cannonade", spec = spell(Direct -> damageCreatures(Damage(19, isSpell = true)))),
    Creature("SteamTank", Some(6), 60, spec = creature(Direct -> damageCreatures(Damage(12, isAbility = true))))))

  private def initCard(house: House, houseIndex : Int, costFn: Int => Int) {
    house.cards.zipWithIndex.foreach {
      case (c, i) =>
        c.cost = costFn(i)
        c.houseIndex = houseIndex
    }
  }

  val base = List(Fire, Water, Air, Earth)
  val special = List(Mecanic)
  val specialNames = special.map(_.name).to[Set]
  val list = base ++ special

  def isSpecial(house : House)= specialNames.contains(house.name)

  val basicCostFonction = { i: Int => i + 1 }
  base.zipWithIndex.foreach{ case (house, index) => initCard(house, index, basicCostFonction) }
  special.foreach(h => initCard(h, 4, { i: Int =>
    if (i == 0) i else i + 1
  }))
}
