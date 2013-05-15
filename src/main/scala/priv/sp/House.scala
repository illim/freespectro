package priv.sp

import house._

object House {
  val currentId = new java.util.concurrent.atomic.AtomicInteger
}

case class House(name: String, cards: List[Card], houseIndex : Int = 4, effects : List[CardSpec.PhaseEffect] = Nil){
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
  def manaGens = List((0, 3), (1, 5), (3, 5))
}

// hacks for serialization
object HouseSingleton extends Houses

class Houses
  extends Fire with Water with Air with EarthHouse with Mecanic
  with ZenMage with Sower with JunkMage /**  with LostChurch*/ {
  import CardSpec._
  import GameCardEffect._

  val base = List(Fire, Water, Air, Earth)
  val special = List(Mecanic, Sower, Zen, Junk)//, Junk, LostChurch)
  val specialNames = special.map(_.name).to[Set]
  private val allHouses = base ++ special
  private val allCards = allHouses.flatMap(_.cards)

  val getHouseById = allHouses.map(h => h.houseId -> h).toMap
  def getCardById(id : Int) : Card = allCards.find(_.id == id).getOrElse(sys.error(s"card id $id not found "))

  def isSpecial(house : House)= specialNames.contains(house.name)

  base.zipWithIndex.foreach{ case (house, index) => house.initCards(Houses.basicCostFunc) }
}
