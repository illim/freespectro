package priv.sp

import util.Random.shuffle

class DummyBot {

  def executeAI(player : PlayerState) = {
    val firstCreature = shuffle(player.houseCards).view.flatMap { houseCard =>
      shuffle(houseCard.cardStates).collectFirst {
        case cardState @ HouseCardState(c: Creature, _) if cardState.isAvailable =>
          c
      }
    }.headOption
    firstCreature.flatMap { c =>
      val slots = player.slots
      shuffle(0 to 5).find(!slots.contains(_)).map { num =>
        val cardState = CardState.creature(c)
        Command(opponent, c, List(OwnerSlot(num)))
      }
    }
  }
}