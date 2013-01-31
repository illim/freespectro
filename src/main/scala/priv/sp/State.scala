package priv.sp

import util.Random

case class GameState(players: List[PlayerState], phase: Phase)

trait Phase
case class Waiting(player: PlayerId) extends Phase
case class Running(commandOption: Option[Command], player: PlayerId) extends Phase

class GameStateMachine(var state: GameState) {
  def this() = this(GameState(List(PlayerState.create(), PlayerState.create()), Waiting(owner)))
  private var onTransitions = List.empty[TransitionHandler]

  def goto(next: Phase) {
    println("phase " + next)
    state = state.copy(phase = next)
    onTransitions.foreach { f =>
      if (f.isDefinedAt(next)) f(next)
    }
    state.phase match {
      case Running(commandOption, player) =>
        // todo
        goto(Waiting(swapPlayer(player)))
      case _ =>
    }
  }

  def onTransition(f: TransitionHandler) { onTransitions ::= f }
}

object PlayerHouse {
  def from(house: House) = PlayerHouse(house, randomize(house), Random.nextInt(3) + 3)

  private def randomize(house: House) = {
    import Random.shuffle

    (shuffle(house.cards.take(6)).take(2)
      ++ shuffle(house.cards.drop(6).take(4)).take(1)
      ++ shuffle(house.cards.drop(10)).take(1)).sortBy(_.cost)
  }
}

case class PlayerHouse(house: House, cards: List[Card], var mana: Int)

object PlayerState {
  def create() = {
    new PlayerState(
      List(
        PlayerHouse.from(Houses.FireHouse),
        PlayerHouse.from(Houses.WaterHouse),
        PlayerHouse.from(Houses.AirHouse),
        PlayerHouse.from(Houses.EarthHouse)))
  }
}
case class CardState(card: Card, house: PlayerHouse) {
  def isAvailable = card.cost <= house.mana
}
case class HouseCards(house: PlayerHouse, cardStates: List[CardState])
class PlayerState(houses: List[PlayerHouse]) {
  val houseCards = houses.map { h =>
    HouseCards(h, h.cards.map(c => CardState(c, h)))
  }
}
