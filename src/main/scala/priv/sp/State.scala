package priv.sp

import util.Random
import collection._

case class GameState(players: List[PlayerState], phase: Phase)

trait Phase
case class Waiting(player: PlayerId) extends Phase
case class Submitting(commandOption: Option[(Command, CardEffects)], player: PlayerId) extends Phase
case class Running(player: PlayerId) extends Phase

class StateRef[A](deref : => A){
  def map[B](f : A => B) = new StateRef(f(deref))
  def get = deref
}

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
      case Submitting(commandOption, player) =>
        commandOption.foreach{ case (command, effects) =>
          effects.effects.foreach{
            case Summoned(card, numSlot) => state.players(player).slots(numSlot) = card
            case _ =>
          }          
        }
        goto(Running(player))
      case Running(player) =>
        // todo
        goto(Waiting(swapPlayer(player)))
      case _ =>
    }
  }

  def onTransition(f: TransitionHandler) { onTransitions ::= f }
  def playerRef(player : PlayerId) = new StateRef(state.players(player))
  def slotRef(player : PlayerId, numSlot : Int) = playerRef(player).map(_.slots.get(numSlot))
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