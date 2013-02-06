package priv.sp

import util.Random.shuffle
import scala.util.continuations._

class DummyBot {

  def executeAI(player : PlayerState) = shift { k: (Option[(Command, CardEffects)] => Unit) =>
    val firstCreature = shuffle(player.houseCards).view.flatMap { houseCard =>
      shuffle(houseCard.cardStates).collectFirst {
        case cardState @ HouseCardState(c: Creature, _) if cardState.isAvailable =>
          c
      }
    }.headOption
    k(firstCreature.flatMap { c =>
      val slots = player.slots
      shuffle(0 to 5).find(!slots.contains(_)).map { num =>
        val cardState = CardState.creature(c)
        (Command(opponent, c, List(OwnerSlot(num))), CardEffects(List(Summoned(cardState, num))))
      }
    })
  }
}