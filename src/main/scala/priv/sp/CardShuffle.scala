package priv.sp

import util.Random

class CardShuffle(sp: SpWorld) {

  def get() = {
    val p1 = createOnePlayer(owner, None)
    val p2 = createOnePlayer(opponent, Some(p1._1))
    List(p1, p2)
  }

  def createOnePlayer(p : PlayerId, exclusion : Option[PlayerDesc]) = {
    val getCardRange = exclusion match {
      case Some(p) => new CardModel.ExcludePlayerCards(p)
      case None => CardModel.BasicCardRange
    }
    val cardModel = CardModel.build(sp.houses, getCardRange)
    new CardShuffler(cardModel).solve()
    val manaModel = new ManaModel(cardModel)
    new ManaShuffler(manaModel, p == owner).solve()
    (cardModel.toPlayerHouseDesc(sp.houses), manaModel.toHouseStates)
  }
}

import oscar.cp.modeling._
import oscar.search._
import oscar.cp.core._

object CardModel {

  def build(spHouses : Houses, getCardRange : GetCardRange = BasicCardRange, cp : CPSolver = CPSolver()) =
    new CardModel(cp, spHouses.list.map(h => new HModel(cp, h, spHouses, getCardRange)))

  trait GetCardRange{ def apply(house : House) : List[Int]  }
  case object BasicCardRange extends GetCardRange {
    def apply(house : House) = house.costs
  }
  class ExcludePlayerCards(p: PlayerDesc) extends GetCardRange {
    val playerCards = p.houses.map{ h => h.house.name -> h.cardList.map(_.cost).to[Set] }.toMap
    def apply(house : House) = {
      playerCards.get(house.name) match {
        case Some(cards) => house.costs.filterNot(cards.contains _)
        case None => house.costs
      }
    }
  }

  def manaGens = List((0, 3), (1, 5), (3, 5))

  // todo why _.randomValue not work?
  def getRandom(x : CPVarInt) = {
    val ind = Random.nextInt(x.size)
    x.toArray.apply(ind)
  }
}

import CardModel._

class CardModel(val cp : CPSolver, val houses : List[HModel]){
  val fire :: water :: air :: earth :: special :: Nil = houses.map(_.cards)
  val allCards = houses.flatMap(_.cards)

  def toPlayerHouseDesc(spHouses : Houses) = PlayerDesc(
    (0 to 4).map{ i =>
      val house = spHouses.list(i)
      val solveds = houses(i).getSolveds
      PlayerHouseDesc(house, house.cards.filter(c => solveds.contains(c.cost)).to[Array])
    }.toArray)
}

class HModel(cp : CPSolver, house : House, spHouses : Houses, getCardRange : GetCardRange){
  val isSpecial = spHouses.isSpecial(house)
  val cards = if (isSpecial) {
    val (c1, ctemp) = range.partition(_ < 5)
    val (c2, c3) = ctemp.partition(_ < 7)
    (CPVarInt(cp, c1)
     :: CPVarInt(cp, c1)
     :: CPVarInt(cp, c2)
     :: CPVarInt(cp, c3) :: Nil)
  } else {
    (0 to 3).map(i => CPVarInt(cp, range))
  }

  def getSolveds = cards.map(_.value).to[Set]
  private def range = getCardRange(house).to[Set]
}

class CardShuffler(cardModel : CardModel) {
  import cardModel._

  def solve() = {
    cp.subjectTo{
      houses.foreach{ house =>
        import house.cards

        if (!house.isSpecial){
          val s = sum(cards)
          cp.add(s < 30)
          cp.add(s > 20)
        }
        cp.add(allDifferent(cards))
      }
      cp.add(oneManaGen)
      cp.add(oneWipe)

      val d = getDefense
      cp.add(d > 1)
      cp.add(d < 4)

      val fs = getFinishers
      cp.add(fs > 0)
      cp.add(fs < 3)

      cp.add(maximum(fire) > 9)
      cp.add(maximum(earth) > 9)
      cp.add(maximum(water) > 8)
      cp.add(maximum(air) > 8)

      // bans
      cp.add(contains(9, fire) ==> notContains(11, fire))
      cp.add(contains(5, fire) ==> notContains(3, earth))
      cp.add(contains(1, water) ==> notContains(9, earth))
      cp.add(contains(5, earth) ==> notContains(6, earth))

    } exploration {
      cp.binary(allCards.toArray, _.size, getRandom _ )
    } run(1)
  }

  def oneManaGen = sum(
    manaGens.map{ case (houseIndex, cost) =>
      contains(cost, houses(houseIndex).cards)
    }) == 1

  def oneWipe = sum( List(
    contains(6, fire),
    contains(9, fire),
    contains(8, air))) == 1

  def getFinishers = sum( List(
    contains(11, fire),
    contains(6, air),
    contains(8, air),
    contains(6, earth)))

  def getDefense = sum( List(
    contains(6, water),
    contains(10, water),
    contains(4, air),
    contains(1, earth),
    contains(2, earth),
    contains(4, earth),
    contains(11, earth)))

  def contains(x : Int, l : Traversable[CPVarInt]) = {
    l.foldLeft(CPVarBool(cp, false)){ (acc, e) =>
      acc || (e === x)
    }
  }

  def notContains(x : Int, l : Traversable[CPVarInt]) = {
    l.foldLeft(CPVarBool(cp, true)){ (acc, e) =>
      acc && (e !== x)
    }
  }
}

class ManaModel(val cardModel : CardModel, val cp : CPSolver = CPSolver()){
  val manas = (0 to 3).map{ _ =>
    CPVarInt(cp, 2 to 6)
  }

  def toHouseStates = (manas.map{ m =>
    new HouseState(m.value)
  } :+ (new HouseState(2))).to[Vector]
}

class ManaShuffler(model : ManaModel, isFirst : Boolean){
  import model._
  val total = if (isFirst) 19 else 18
  val manaGen = findManaGen

  def solve() = {
    cp.subjectTo{
      val s = sum(manas)
      cp.add(s == total)
      manaGen.foreach{ case (idx, cost) =>
        cp.add(manas(idx) >= (if (isFirst) cost else (cost -1)))
      }
    } exploration {
      cp.binary(manas.toArray, _.size, getRandom _ )
    } run(1)
  }

  def findManaGen = {
    cardModel.houses.zipWithIndex.flatMap{ case (h, idx) =>
      val solveds = h.getSolveds
      manaGens.collectFirst{ case (houseIndex, cost) if idx == houseIndex && solveds.contains(cost) =>
        (idx, cost)
      }
    }
  }
}
