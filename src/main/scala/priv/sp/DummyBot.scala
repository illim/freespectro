package priv.sp

import util.Random.shuffle

class DummyBot(gsm: GameStateMachine) {
  val cards = gsm.state.players(opponent)
  val playerStateRef = gsm.playerRef(opponent)
  gsm.onTransition {
    case Waiting(player) if player == opponent =>
      executeAI()
  }

  def executeAI() {
    val firstCreature = shuffle(cards.houseCards).view.flatMap { houseCard =>
      shuffle(houseCard.cardStates).collectFirst {
        case cardState @ HouseCardState(c: Creature, _) if cardState.isAvailable =>
          c
      }
    }.headOption
    val command = firstCreature.flatMap { c =>
      val slots = playerStateRef.get.slots
      shuffle(0 to 5).find(!slots.contains(_)).map { num =>
        val cardState = CardState.creature(c)
        (Command(opponent, c, List(OwnerSlot(num))), CardEffects(List(Summoned(cardState, num))))
      }
    }
    gsm.goto(Submitting(command, opponent))
  }
}