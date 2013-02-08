package priv.sp

import util.Random
import collection._

case class GameState(players: List[PlayerState])


case class PlayerHouse(house: House, cards: List[Card], var mana: Int)

case class HouseCardState(card: Card, house: PlayerHouse) {
  def isAvailable = card.cost <= house.mana
}
case class HouseCards(house: PlayerHouse, cardStates: List[HouseCardState])
class PlayerState(
    houses: List[PlayerHouse],
    var slots : immutable.TreeMap[Int, CardState] = immutable.TreeMap.empty,
    var life : Int = 60) {
  val houseCards = houses.map { h =>
    HouseCards(h, h.cards.map(c => HouseCardState(c, h)))
  }
}

object CardState {
  def creature(card : Card) = {
    card match {
      case creature : Creature => CardState(creature, creature.life, creature.attack getOrElse 0)
      case _ => sys.error(card + " is not a creature")
    }
  }
}
case class CardState(card : Creature, var life : Int, attack : Int, var hasRunOnce : Boolean = false){
  def toggleRunOnce(){if (! hasRunOnce) hasRunOnce = true}
}