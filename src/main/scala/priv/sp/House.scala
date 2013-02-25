package priv.sp

case class House(name: String, cards: List[Card])

class Houses {
  import CardSpec._
  import GameCardEffect._

  val Fire = House("fire", List(
    Creature("GoblinBerserker", Some(4), 16),
    Creature("WallofFire", Some(0), 5, spec = creature(Direct -> DamageCreatures(5))),
    Creature("PriestOfFire", Some(3), 13),
    Creature("FireDrake", Some(4), 18),
    Creature("OrcChieftain", Some(3), 16),
    Spell("FlameWave", spec = spell(Direct -> DamageCreatures(9))),
    Creature("MinotaurCommander", Some(6), 20),
    Creature("Bargul", Some(8), 25, spec = creature(Direct -> MassDamageCreatures(4))),
    Spell("Inferno",
      inputSpec = Some(SelectTargetCreature),
      spec = spell(Direct -> Custom { env: Env =>
        import env._

        otherPlayer.slots.%== {
          _.map {
            case (num, slot) =>
              num -> SlotState.lifeL.mod(_ - (if (num == selected) 18 else 10), slot)
          }
        }
      })),
    Creature("FireElemental", None, 36),
    Spell("Armageddon"),
    Creature("Dragon", Some(9), 41)))

  val Water = House("water", List(
    Spell("Meditation"),
    Creature("SeaSprite", Some(5), 22),
    Creature("MerfolkApostate", Some(3), 10),
    Creature("IceGolem", Some(4), 12),
    Creature("MerfolkElder", Some(3), 16),
    Creature("IceGuard", Some(3), 20),
    Creature("GiantTurtle", Some(5), 17),
    Spell("AcidicRain", spec = spell(Direct -> MassDamageCreatures(15))),
    Creature("MerfolkOverlord", Some(7), 34),
    Creature("WaterElemental", None, 38),
    Creature("MindMaster", Some(6), 22),
    Creature("AstralGuard", Some(1), 17)))

  val Air = House("air", List(
    Creature("FaerieApprentice", Some(4), 11),
    Creature("Griffin", Some(3), 15),
    Spell("CalltoThunder",
      inputSpec = Some(SelectTargetCreature),
      spec = spell(Direct -> MassDamageCreatures(6), Direct -> Damage(6))),
    Creature("FaerieSage", Some(4), 19),
    Creature("WallOfLightning", Some(0), 28),
    Spell("LightningBolt"),
    Creature("Phoenix", Some(6), 18),
    Spell("ChainLightning", spec = spell(Direct -> DamageCreatures(9), Direct -> Damage(9))),
    Creature("LightningCloud", Some(4), 20),
    Spell("Tornado"),
    Creature("AirElemental", None, 44),
    Creature("Titan", Some(9), 40)))

  val Earth = House("earth", List(
    Creature("ElvenHealer", Some(2), 12),
    Spell("NaturesRitual"),
    Creature("ForestSprite", Some(1), 22),
    Spell("Rejuvenation"),
    Creature("elfHermit", Some(1), 13),
    Spell("NaturesFury"),
    Creature("GiantSpider", Some(4), 21),
    Creature("Troll", Some(6), 25),
    Spell("StoneRain"),
    Creature("EarthElemental", None, 49),
    Creature("MasterHealer", Some(3), 35),
    Creature("Hydra", Some(3), 40)))

  val Mecanic = House("Mechanics", List(
    Spell("Overtime"),
    Creature("DwarvenRifleman", Some(4), 17),
    Creature("DwarvenCraftsman", Some(2), 17),
    Creature("Ornithopter", Some(4), 24),
    Creature("SteelGolem", Some(6), 20),
    Creature("Cannon", Some(8), 29),
    Spell("Cannonade"),
    Creature("SteamTank", Some(6), 60)))

  private def initCost(costFn: Int => Int)(house: House) {
    house.cards.zipWithIndex.foreach {
      case (c, i) =>
        c.cost = costFn(i)
    }
  }

  val base = List(Fire, Water, Air, Earth)
  val special = List(Mecanic)
  val specialNames = special.map(_.name).to[Set]
  val list = base ++ special
  
  def isSpecial(house : House)= specialNames.contains(house.name)

  val basicCostFonction = { i: Int => i + 1 }
  base.foreach(initCost(basicCostFonction))
  special.foreach(initCost({ i: Int =>
    if (i == 0) i else i + 1
  }))
}
