package priv.sp

import collection._
import util.Random

class CardShuffle(houses : Houses) {

  def get(specialHouses : List[House], startingPlayer : PlayerId = owner) = {
    val p1 = createPlayer(owner, specialHouses(owner), startingPlayer)
    val p2 = createPlayer(opponent, specialHouses(opponent), startingPlayer, Some(p1._1))
    List(p1, p2)
  }

  def createPlayer(p : PlayerId, specialHouse : House, startingPlayer : PlayerId, exclusion : Option[PlayerDesc] = None) = {
    Random.setSeed(System.currentTimeMillis)
    val getCardRange = exclusion match {
      case Some(p) => new CardModel.ExcludePlayerCards(p)
      case None => CardModel.BasicCardRange
    }
    val cardModel = CardModel.build(houses, specialHouse, getCardRange)
    val pDesc = new CardShuffler(cardModel).solve()
    val manaModel = new ManaModel(cardModel)
    val houseStates = new ManaShuffler(manaModel, pDesc, p == startingPlayer).solve()
    (pDesc, houseStates)
  }
}

import oscar.cp.modeling._
import oscar.cp.core._
import priv.util.CpHelper
import priv.util.CpHelper._

object CardModel {

  def build(houses : Houses, specialHouse : House, getCardRange : GetCardRange = BasicCardRange, cp : CPSolver = CPSolver()) =
    new CardModel(cp, (houses.base :+ specialHouse).map(h => new HModel(cp, h, houses, getCardRange)))

  trait GetCardRange{ def apply(house : House) : List[Int]  }
  case object BasicCardRange extends GetCardRange {
    def apply(house : House) = house.costs
  }
  class ExcludePlayerCards(p: PlayerDesc) extends GetCardRange {
    val playerCards = p.houses.map{ h => h.house.name -> h.cards.map(_.cost).to[immutable.Set] }.toMap
    def apply(house : House) = {
      playerCards.get(house.name) match {
        case Some(cards) => house.costs.filterNot(cards.contains _)
        case None => house.costs
      }
    }
  }
}

import CardModel._

class CardModel(val cp : CPSolver, val houses : List[HModel]){
  val fire :: water :: air :: earth :: special :: Nil = houses.map(_.cards)
  val allCards = houses.flatMap(_.cards)

  def toPlayerHouseDesc = PlayerDesc(
    (0 to 4).map{ i =>
      val house = houses(i).house
      val solveds = houses(i).getSolveds
      PlayerHouseDesc(house, house.cards.filter(c => solveds.contains(c.cost)).map(CardDesc(_))(breakOut))
    }(breakOut) : Vector[PlayerHouseDesc])
}

/**
 * this could have been modeled differently with a list of boolean like in cardguess.
 * Dunno what's better. The alldifferent constraint could be a bit consuming, on the other side there's less variables.
 */
class HModel(cp : CPSolver, val house : House, spHouses : Houses, getCardRange : GetCardRange){
  val isSpecial = spHouses.isSpecial(house)
  val cards = if (isSpecial) {
    val (c0, ctemp) = range.partition(_ < 3)
    val (c1, ctemp2) = ctemp.partition(_ < 5)
    val (c2, c3) = ctemp2.partition(_ < 7)
    (CPIntVar(cp, c0)
     :: CPIntVar(cp, c1)
     :: CPIntVar(cp, c2)
     :: CPIntVar(cp, c3) :: Nil)
  } else {
    (0 to 3).map(i => CPIntVar(cp, range))
  }

  def getSolveds : Set[Int] = cards.map(_.value)(breakOut)
  private def range = getCardRange(house).to[immutable.Set]
}

class CardShuffler(cardModel : CardModel) extends CpHelper {
  implicit def solver = cardModel.cp
  import cardModel._

  def solve(timeLimit : Int = Int.MaxValue) = {
    val vars = allCards
    houses.foreach{ house =>
      import house.cards

      if (!house.isSpecial){
        val s = sum(cards)
        add(s < 30)
        add(s > 20)
      }
      add(allDifferent(cards))
    }
    add(oneManaGen)
    add(oneWipe)

    val d = getDefense
    add(d > 1)
    add(d < 4)

    val fs = getFinishers
    add(fs > 0)
    add(fs < 3)

    add(maximum(fire) > 9)
    add(maximum(earth) > 9)
    add(maximum(water) > 8)
    add(maximum(air) > 8)

    // bans
    add(contains(9, fire) ==> notContains(11, fire))
    add(contains(5, fire) ==> notContains(3, earth))
    add(contains(1, water) ==> notContains(9, earth))
    add(contains(5, earth) ==> notContains(6, earth))

    search {
      binaryFirstFail(vars, getRandom _ )
    }
    var playerDesc : PlayerDesc = null
    onSolution {
      playerDesc = toPlayerHouseDesc
    }
    // maybe this can be considered as a hack and hide a real problem
    // or is it just a variable relaxing method by retrying and hoping random doesn't lead to a dead end
    // i don't really know (todo use vizualisation tool)
    softRun(vars, timeLimit)
    playerDesc
  }

  def oneManaGen = sum(
    Houses.manaGens.map{ case (houseIndex, cost) =>
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
}

class ManaModel(val cardModel : CardModel, val cp : CPSolver = CPSolver()){
  val manas = (0 to 3).map{ _ =>
    CPIntVar(cp, 2 to 6)
  }

  def toHouseStates = (manas.map{ m =>
    new HouseState(m.value)
  } :+ (new HouseState(2))).to[Vector]
}

class ManaShuffler(model : ManaModel, pDesc : PlayerDesc, isFirst : Boolean) {
  import model._

  implicit val solver = model.cp
  val total = if (isFirst) 19 else 18
  val manaGen = findManaGen

  def solve() = {
    val s = sum(manas)
    add(s == total)
    manaGen.foreach{ case (idx, cost) =>
      add(manas(idx) < 7)
      add(manas(idx) >= (if (isFirst) cost else (cost -1)))
    }

    search {
      binaryFirstFail(manas, getRandom _ )
    }

    var houseStates : Vector[HouseState] =null
    onSolution {
      houseStates = toHouseStates
    }
    start(nSols = 1)
    houseStates
  }

  def findManaGen = {
    pDesc.houses.zipWithIndex.flatMap{ case (h, idx) =>
      val costs : Set[Int] = h.cards.map(_.cost)(breakOut)
      Houses.manaGens.collectFirst{ case (houseIndex, cost) if idx == houseIndex && costs.contains(cost) =>
        (idx, cost)
      }
    }
  }
}
