package priv.sp.bot

import priv.sp._
import collection._
import util.Random

/**
 * the ai opponent is deduced from the known cards and shuffle rules
 * he has more cards than the normal player so the ai can try all possible moves. One weak point
 * is that currently there's no difference between a 100%certain and a 50%probable card for the ai.
 */
class CardGuess(gameDesc : GameDesc, sp : SpWorld) {

  def createAIPlayer(botPlayerId : PlayerId, knownCards : Set[(Card, Int)], timeLimit : Int = Int.MaxValue) : Option[PlayerDesc] = {
    val knowledge = new ModelFilter(knownCards, gameDesc.players(botPlayerId))
    val cardModel = GCardModel.build(sp.houses, knowledge)
    new CardGuesser(cardModel).solve(timeLimit)
    if (cardModel.cp.isFailed) None
    else Some(cardModel.toPlayerHouseDesc(sp.houses))
  }
}

class ModelFilter(val knownCards : Set[(Card, Int)], p: PlayerDesc) {
  val playerCards = p.houses.map{ h => h.house.name -> h.cardList.map(_.cost).to[Set] }.toMap

  def getExclusions(house : House) = {
    playerCards.get(house.name) match {
      case Some(cards) => cards
      case None => Nil
    }
  }
}

import oscar.cp.modeling._
import oscar.search._
import oscar.cp.core._
import priv.util.CpHelper
import priv.util.CpHelper._

object GCardModel {
  def build(spHouses : Houses, knowledge : ModelFilter, cp : CPSolver = CPSolver()) =
    new GCardModel(cp, spHouses.list.map(h => new GHModel(cp, h, spHouses, knowledge)))
}

class GCardModel(val cp : CPSolver, val houses : List[GHModel]){
  val fire :: water :: air :: earth :: special :: Nil = houses
  val allCards = houses.flatMap(_.cards)

  def toPlayerHouseDesc(spHouses : Houses) = PlayerDesc(
    (0 to 4).map{ i =>
      val house = spHouses.list(i)
      val solveds = houses(i).getSolveds
      println("house " + house.name + " : " + solveds.toList)
      PlayerHouseDesc(house, house.cards.filter(c => solveds.contains(c)).to[Array])
    }.toArray)
}

class GHModel(cp : CPSolver, house : House, spHouses : Houses, knowledge : ModelFilter){
  private val costToIndex = house.cards.zipWithIndex.map{ case (c, i) =>
    c.cost -> i
  }.toMap
  // 0 : exclude, 1: sure, 2 : maybe
  val cards = Vector.fill(house.cards.size)(CPVarInt(cp, 0 to 2))
  val knownCards = knowledge.knownCards.filter{ case (card, _) => spHouses.list(card.houseIndex) == house}.toList.sortBy(_._2)
  knownCards.foreach{ case (card, idx) => apply(card.cost).assign(1) }

  /**
   * Exclusion relative to card position (ex: if sea sprite is on 0, the player don't have meditation)
   */
  val (lastidx, lastcost) = ((-1, 0) /: knownCards){ case ((lastidx, lastcost), (card, idx)) =>
    excludeKnownInterval(lastidx, lastcost, idx, card.cost)
    (idx, card.cost)
  }
  excludeKnownInterval(lastidx, lastcost, 4, house.cards.map(_.cost).max + 1)
  knowledge.getExclusions(house).foreach{cost => apply(cost).assign(0)}

  def apply(cost : Int) = cards(costToIndex(cost))
  def excludeKnownInterval(lastidx : Int, lastcost : Int, idx : Int, cost : Int ) {
    if (idx == lastidx + 1) {
      ((lastcost + 1) until cost).flatMap(costToIndex.get _ ).foreach{ i => cards(i).assign(0) }
    }
  }
  def getSolveds = house.cards.zipWithIndex.collect{ case (card, idx) if cards(idx).value >= 1 =>
    card
  }.to[Set]
}

class CardGuesser(cardModel : GCardModel) extends CpHelper {
  def cp = cardModel.cp

  import cardModel._

  def solve(timeLimit : Int = Int.MaxValue) = {
    cp.timeLimit = timeLimit
    cp.subjectTo{
      cp.add(oneManaGen)
      cp.add(oneWipe)

      // bans
      cp.add(is(fire(9))  ==> not(fire(11)))
      cp.add(is(fire(5))  ==> not(earth(3)))
      cp.add(is(water(1)) ==> not(earth(9)))
      cp.add(is(earth(5)) ==> not(earth(6)))
    } exploration {
      cp.binary(allCards.toArray, _.size, _.max)
    } run(1)
  }

  def is(x : CPVarInt) = (x === 1)
  def not(x : CPVarInt) = (x === 0)

  def oneManaGen = {
    val manaGens = Houses.manaGens.map{ case (houseIndex, cost) =>
      houses(houseIndex).apply(cost)
    }
    contains(1, manaGens ) ==> (sum(manaGens) === 1)
  }

  def oneWipe = {
    val wipes = List(
    fire(6),
    fire(9),
    fire(8))
    contains(1, wipes) ==> (sum(wipes) === 1)
  }
}
