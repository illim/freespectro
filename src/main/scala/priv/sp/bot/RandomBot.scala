package priv.sp.bot

import priv.sp._
import util.Random.shuffle
import priv.sp.Creature
import priv.sp.PlayerState

class RandomBot(playerId: PlayerId) extends Bot {

  def executeAI(state: GameState) = {
    val player = state.players(playerId)
    val firstCreature = shuffle(player.houses).view.flatMap { house =>
      shuffle(house.cards).collectFirst {
        case card: Creature if card.isAvailable(house) =>
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