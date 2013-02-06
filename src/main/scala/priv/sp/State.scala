package priv.sp

import util.Random
import collection._

case class GameState(players: List[PlayerState])

class StateView[A](read : => A){
  def map[B](f : A => B) = new StateView(f(read))
  def get = read
}

case class PlayerHouse(house: House, cards: List[Card], var mana: Int)

case class HouseCardState(card: Card, house: PlayerHouse) {
  def isAvailable = card.cost <= house.mana
}
case class HouseCards(house: PlayerHouse, cardStates: List[HouseCardState])
class PlayerState(houses: List[PlayerHouse], val slots : mutable.Map[Int, CardState] = mutable.Map.empty) {
  val houseCards = houses.map { h =>
    HouseCards(h, h.cards.map(c => HouseCardState(c, h)))
  }
}

object CardState {
  def creature(card : Card) = {
    card match {
      case creature : Creature => CardState(card, creature.life, creature.attack getOrElse 0)
      case _ => sys.error(card + " is not a creature")
    }
  }
}
case class CardState(card : Card, life : Int, attack : Int, hasRunOnce : Boolean = false)