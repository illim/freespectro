package priv.sp

import util.Random.shuffle

class DummyBot {

  def executeAI(player : PlayerState) = {
    val firstCreature = shuffle(player.houses).view.flatMap { house =>
      shuffle(house.cards).collectFirst {
        case card : Creature if card.isAvailable(house) =>
          card
      }
    }.headOption
    firstCreature.flatMap { c =>
      val slots = player.slots
      shuffle(0 to 5).find(!slots.contains(_)).map { num =>
        Command(opponent, c, List(OwnerSlot(num)))
      }
    }
  }
}