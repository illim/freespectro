package priv.sp

case class House(name: String, cards: List[Card]){
  def costs = cards.map(_.cost)
  override def hashCode() : Int = name.hashCode
  override def equals(o : Any) = {
    o match {
      case h : House => h.hashCode() == hashCode()
      case _ => false
    }
  }
}

object Houses {
  val forestSpider = new Creature("ForestSpider", Some(2), 11){
    cost = 1
    houseIndex = 2
  }

  def manaGens = List((0, 3), (1, 5), (3, 5))
}

// hacks for serialization
object HouseSingleton extends Houses
class FireDrake extends Creature("FireDrake", Some(4), 18, "Attack the turn he is summoned") {
  override val runOnce = true
}
class GiantTurtle extends Creature ("GiantTurtle", Some(5), 17, "Absorb 5 damage"){
  override def inflict(damage : Damage, life : Int) = life - math.max(0, damage.amount - 5)
}
class SteelGolem extends Creature("SteelGolem", Some(6), 20, "Immune to spell and absorb 1 damage", immune = true){
  override def inflict(damage : Damage, life : Int) = if (damage.isEffect) life else (life - math.max(0, damage.amount - 1))
}

class Houses extends HouseCardEffects {
  import CardSpec._
  import GameCardEffect._

  val Fire = House("fire", List(
    Creature("Goblin", Some(4), 16, "Every turn deals 2 damage to owner adjacent cards", spec = creature(OnTurn ->goblinBerserker)),
    Creature("WallofFire", Some(0), 5, "Deals 5 damage to opponent creatures when summoned", spec = creature(Direct -> damageCreatures(Damage(5, isAbility = true)))),
    Creature("FireMonk", Some(3), 13, "Every turn increase fire mana growth by 1", spec = creature(OnTurn -> addMana(1, 0)), isFocusable = false),
    new FireDrake,
    Creature("OrcChieftain", Some(3), 16, "Increase attack of adjacent card by 2", boardEffect = Some(AddAttack(2, around = true))),
    Spell("FlameWave", "Deals 9 damage to opponent creatures", spec = spell(Direct -> damageCreatures(Damage(9, isSpell = true)))),
    Creature("BullCommander", Some(6), 20, "Increase attack of owner card by 1", boardEffect = Some(AddAttack(1))),
    Creature("Bargul", Some(8), 25, "Deals 4 damage to every creature when summoned", spec = creature(Direct -> massDamage(Damage(4, isAbility = true)))),
    Spell("Inferno", "Deals 18 damage to target and 10 to other opponent creatures", inputSpec = Some(SelectTargetCreature), spec = spell(Direct -> inferno)),
    Creature("FireElemental", None, 36, "Fire Elemental deals 3 damage to opponent creatures when summoned", spec = creature(Direct -> damageCreatures(Damage(3, isAbility = true)), OnTurn -> addMana(1, 0))),
    Spell("Armageddon", "Damage any creature and opponent by 8 + fire mana", spec = spell(Direct -> armageddon)),
    Creature("Dragon", Some(9), 41, "Increase spell damage by 50%", mod = Some(new SpellMod(x => math.ceil(x * 1.5).intValue)))))

  val Water = House("water", List(
    Spell("Meditate", "Increase normal mana by 1", spec = spell(Direct -> addMana(1, 0, 2, 3))),
    Creature("seasprite", Some(5), 22, "Every turn deals 2 damage to owner",
      spec = creature(OnTurn -> { env : Env => env.player.life.%==( _ - env.guardSelf(2)) })),
    Creature("MerfolkApostate", Some(3), 10, "Give 2 fire mana when summoned", spec = creature(Direct -> addMana(2, 0))),
    Creature("IceGolem", Some(4), 12, "Immune to spell & ability", immune = true),
    Creature("MerfolkElder", Some(3), 16, "Increase air mana growth by 1", spec = creature(OnTurn -> addMana(1, 2)), isFocusable = false),
    Creature("IceGuard", Some(3), 20, "Halve damage dealt to owner", mod = Some(new SpellProtectOwner(x => math.ceil(x / 2.0).intValue))),
    new GiantTurtle,
    Spell("AcidicRain", "Damage all creature by 15 and decrease mana of opponent by 1", spec = spell(Direct -> massDamage(Damage(15, isSpell = true)), Direct -> { env : Env =>
      env.otherPlayer.houses.%=={ houses => HouseState.incrMana(houses, -1 , 0, 1, 2, 3, 4) }
    })),
    Creature("MerfolkOverlord", Some(7), 34, "Adjacent cards attack the turn they're summoned", boardEffect = Some(ToggleRunAround)),
    Creature("WaterElemental", None, 38, "Heals owner by 10 when summoned", spec = creature(Direct -> heal(10), OnTurn -> addMana(1, 1))),
    Creature("MindMaster", Some(6), 22, "Increase mana growth by 1", spec = creature(OnTurn -> addMana(1, 0, 1, 2, 3, 4))),
    Creature("AstralGuard", Some(1), 17, "Decrease mana growth by 1", spec = creature(OnTurn -> { env : Env =>
      env.otherPlayer.houses.%=={ houses => HouseState.incrMana(houses, -1 , 0, 1, 2, 3, 4) }
    }))))

  val Air = House("air", List(
    Creature("FaerieApprentice", Some(4), 11, "Increase spell damage by 1", mod = Some(new SpellMod(x => x + 1))),
    Creature("Griffin", Some(3), 15, "If Air mana >= 5 deals 5 damage to opponent when summoned",
      spec = creature(Direct -> { env : Env =>
        if (env.getMana(2) > 4) env.otherPlayer.life.%==( _ - env.guard(5)) else GameState.unit
      })),
    Spell("CalltoThunder", "Deals 6 damage to target creature and opponent",
      inputSpec = Some(SelectTargetCreature),
      spec = spell(Direct -> damageCreature(Damage(6, isSpell = true)), Direct -> damage(Damage(6, isSpell = true)))),
    Creature("FaerieSage", Some(4), 19, "When summoned heals owner by amount of earth mana(limited to 10)",
      spec = creature(Direct -> { env : Env =>
        env.player.life.%==( _ + math.min(env.getMana(3), 10))
      })),
    Creature("WallOfLightning", Some(0), 28, "Every turn deals 4 damage to opponent", spec = creature(OnTurn -> damage(Damage(4, isAbility = true)))),
    Spell("Lightnin", "Deals (5 + air power) damage to opponent",
      spec = spell(Direct -> { env : Env => env.otherPlayer.life.%==( _ - env.guard(env.mod(Damage(5 + env.getMana(2), isSpell = true)).amount))})),
    Creature("Phoenix", Some(6), 18, "Can reborn if fire mana >= 10", boardEffect = Some(Reborn{ p =>
      p.houses(0).mana > 9
    })),
    Spell("ChainLightning", "Deals 9 damage to opponent and his creatures", spec = spell(Direct -> damageCreatures(Damage(9, isSpell = true)), Direct -> damage(Damage(9, isSpell = true)))),
    Creature("Cloud", Some(4), 20, "Attack all opponent creatures", multipleTarget = true),
    Spell("Tornado", "destroy target",
      inputSpec = Some(SelectTargetCreature),
      spec = spell(Direct -> { env: Env => env.otherPlayer.slots.%==( _ - env.selected) })),
    Creature("AirElemental", None, 44, "Air elemental deals 8 damage to opponent when summoned", spec = creature(Direct -> damage(Damage(8, isAbility = true)), OnTurn -> addMana(1, 2))),
    Creature("Titan", Some(9), 40, "Deals 15 damage to opposite creature when summoned",
      spec = creature(Direct -> { env : Env => SlotState.inflictCreature(env.otherPlayer, env.selected, Damage(15, isAbility = true))}))))

  val Earth = House("earth", List(
    Creature("ElvenHealer", Some(2), 12, "Heals owner by 2 every turn", spec = creature(OnTurn -> heal(2))),
    Spell("NaturesRitual", "heals target and owner by 8",
      inputSpec = Some(SelectOwnerCreature),
      spec = spell(Direct -> heal(8), Direct -> healCreature(8))),
    Creature("ForestSprite", Some(1), 22, "attack all opponent creatures", multipleTarget = true),
    Spell("Rejuvenation", "heals by 2 * earth mana",
      spec = spell(Direct -> { env : Env => env.player.life.%==( _ + 2 * env.getMana(3))})),
    Creature("elfHermit", Some(1), 13, "Increase earth mana growth by 2", spec = creature(OnTurn -> addMana(2, 3)), isFocusable = false),
    Spell("NaturesFury", "Deals to opponent the sum of the attacks of the 2 strongest owner creatures", spec = spell(Direct -> fury)),
    Creature("HugeSpider", Some(4), 21, "Spawn 2 forest spiders around him", spec = creature(Direct -> spider)),
    Creature("Troll", Some(6), 25, "Every turn heals himself by 4", spec = creature(OnTurn -> healCreature(4))),
    Spell("StoneRain", "Deals 25 damage to any creature", spec = spell(Direct -> massDamage(Damage(25, isSpell = true)))),
    Creature("EarthElemental", None, 49, spec = creature(OnTurn -> addMana(1, 2))),
    Creature("MasterHealer", Some(3), 35, "Every turn heals by 3 owner and his creatures",
      spec = creature(OnTurn -> heal(3), OnTurn -> healCreatures(3))),
    Creature("Hydra", Some(3), 40, "Attack all opponent creatures",
      multipleTarget = true,
      spec = creature(OnTurn -> healCreature(4)))))

  val Mecanic = House("Mechanics", List(
    Spell("Overtime", "Increase mechanic mana by 1", spec = spell(Direct -> addMana(1, 4))),
    Creature("DwarvenRifleman", Some(4), 17, "Deals 4 damage to summoned opponent creatures", mod = Some(InterceptSpawn(Damage(4, isAbility = true)))),
    Creature("DwarvenCraftsman", Some(2), 17, "Increase mechanic mana growth by 1", spec = creature(OnTurn -> addMana(1, 4)), isFocusable = false),
    Creature("Ornithopter", Some(4), 24, "Every turn deals 2 damage to opponent creatures", spec = creature(OnTurn -> damageCreatures(Damage(2, isAbility = true)))),
    new SteelGolem,
    Creature("Cannon", Some(8), 29, "Every turn deals 8 damage to opponent creature with most life", spec=creature(OnTurn -> cannon)),
    Spell("Cannonade", "Deals 19 damage to opponent creatures", spec = spell(Direct -> damageCreatures(Damage(19, isSpell = true)))),
    Creature("SteamTank", Some(6), 60, "When summoned deals 12 damage to opponent creatures", spec = creature(Direct -> damageCreatures(Damage(12, isAbility = true))))))

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
  private val allCards = list.flatMap(_.cards)
  def getCardById(id : Int) : Card = allCards.find(_.id == id).getOrElse(sys.error(s"card id $id not found "))

  def isSpecial(house : House)= specialNames.contains(house.name)

  val basicCostFonction = { i: Int => i + 1 }
  base.zipWithIndex.foreach{ case (house, index) => initCard(house, index, basicCostFonction) }
  special.foreach(h => initCard(h, 4, { i: Int =>
    if (i == 0) i else i + 1
  }))
}
