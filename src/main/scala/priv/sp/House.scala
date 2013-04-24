package priv.sp

import house._

object House {
  val currentId = new java.util.concurrent.atomic.AtomicInteger
}

case class House(name: String, cards: List[Card], houseIndex : Int = 4){
  val houseId = House.currentId.incrementAndGet()

  def costs = cards.map(_.cost)

  def initCards(costFn: Int => Int) {
    cards.zipWithIndex.foreach { case (c, i) =>
      c.cost = costFn(i)
      c.houseIndex = houseIndex
      c.houseId = houseId
    }
  }

  override def toString() = name
  override def hashCode() : Int = name.hashCode
  override def equals(o : Any) = {
    o match {
      case h : House => h.hashCode() == hashCode()
      case _ => false
    }
  }
}

object Houses {
  val basicCostFunc = { i: Int => i + 1 }
  val forestSpider = new Creature("ForestSpider", Some(2), 11){
    cost = 1
    houseIndex = 2
    houseId = 2
  }

  def manaGens = List((0, 3), (1, 5), (3, 5))
}

// hacks for serialization
object HouseSingleton extends Houses

class Houses
  extends Fire with Water with Earth with Mecanic with JunkMage {
  import CardSpec._
  import GameCardEffect._

  val Air = House("air", List(
    Creature("FaerieApprentice", Some(4), 11, "Increase spell damage by 1", mod = Some(new SpellMod(x => x + 1))),
    Creature("Griffin", Some(3), 15, "If Air mana >= 5 deals 5 damage to opponent when summoned",
      effects = effects(Direct -> { env : Env =>
        if (env.getMana(2) > 4) env.otherPlayer.inflict(Damage(5))
      })),
    Spell("CalltoThunder", "Deals 6 damage to target creature and opponent",
      inputSpec = Some(SelectTargetCreature),
      effects = effects(Direct -> damageCreature(Damage(6, isSpell = true)), Direct -> damage(Damage(6, isSpell = true)))),
    Creature("FaerieSage", Some(4), 19, "When summoned heals owner by amount of earth mana(limited to 10)",
      effects = effects(Direct -> { env : Env =>
        env.player.heal(math.min(env.getMana(3), 10))
      })),
    Creature("WallOfLightning", Some(0), 28, "Every turn deals 4 damage to opponent", effects = effects(OnTurn -> damage(Damage(4, isAbility = true)))),
    Spell("Lightnin", "Deals (5 + air power) damage to opponent",
      effects = effects(Direct -> { env : Env => env.otherPlayer.inflict(Damage(5 + env.getMana(2), isSpell = true))})),
    Creature("Phoenix", Some(6), 18, "Can reborn if fire mana >= 10", boardEffect = Some(Reborn{ p =>
      p.houses(0).mana > 9
    })),
    Spell("ChainLightning", "Deals 9 damage to opponent and his creatures", effects = effects(Direct -> damageCreatures(Damage(9, isSpell = true)), Direct -> damage(Damage(9, isSpell = true)))),
    Creature("Cloud", Some(4), 20, "Attack all opponent creatures", multipleTarget = true),
    Spell("Tornado", "destroy target",
      inputSpec = Some(SelectTargetCreature),
      effects = effects(Direct -> { env: Env => env.otherPlayer.slots.destroy(env.selected) })),
    Creature("AirElemental", None, 44, "Air elemental deals 8 damage to opponent when summoned", effects = effects(Direct -> damage(Damage(8, isAbility = true)), OnTurn -> addMana(1, 2))),
    Creature("Titan", Some(9), 40, "Deals 15 damage to opposite creature when summoned",
      effects = effects(Direct -> { env : Env => env.otherPlayer.slots.inflictCreature(env.selected, Damage(15, isAbility = true))})))
      , houseIndex = 2)

  val base = List(Fire, Water, Air, Earth)
  val special = List(Mecanic, Junk)
  val specialNames = special.map(_.name).to[Set]
  private val allHouses = base ++ special
  private val allCards = allHouses.flatMap(_.cards)

  val getHouseById = allHouses.map(h => h.houseId -> h).toMap
  def getCardById(id : Int) : Card = allCards.find(_.id == id).getOrElse(sys.error(s"card id $id not found "))

  def isSpecial(house : House)= specialNames.contains(house.name)

  base.zipWithIndex.foreach{ case (house, index) => house.initCards(Houses.basicCostFunc) }
}
