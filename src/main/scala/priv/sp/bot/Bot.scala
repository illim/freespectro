package priv.sp.bot

import priv.sp._

trait Bot {

  def executeAI(state: GameState): Option[Command]
}

trait ExtBot {
  def game: Game
  def botPlayerId: PlayerId

  // stupid stuff, ai don't know what we have so we create a fake player
  // todo give him all possible cards
  val fakePlayer = game.shuffle.createOnePlayer(game.shuffle.filterHouses(game.desc.players(botPlayerId))(game.sp.houses.list))
  val ripPlayerState = GameDesc.playerLens(other(botPlayerId))%==( desc => GameDesc.replaceCards(fakePlayer._1.houses)(desc)._1)
  val rippedDesc = ripPlayerState.exec(game.desc)

  val gameCard = new GameCard(rippedDesc, game)

  def simulateCommand(state: GameState, command: Command) = {
    def applyEffect(st : GameState) =
      (st /: gameCard.getCommandEffect(command)) { (acc, f) =>
        f.exec(acc)
      }

    val playerId = command.player
    val commandState = gameCard.debitAndSpawn(command).exec(applyEffect(state))
    val runState = (commandState /: commandState.players(playerId).slots) {
      case (st, (numSlot, slot)) =>
        game.runSlot(playerId, numSlot, slot).exec(st)
    }
    game.prepareNextTurn(other(playerId)).exec(runState)
  }
}

class Choices(bot : ExtBot) {

  def getNexts(state: GameState, playerId: PlayerId): Stream[Command] = {
    val slots = state.players(playerId).slots
    val emptySlots = slotRange.filter(num => !slots.isDefinedAt(num))
    val otherSlots = state.players(other(playerId)).slots

    bot.rippedDesc.players(playerId).houses.flatMap { houseDesc  =>
      val houseState = state.players(playerId).houses(houseDesc.index)

      houseDesc.cardList.withFilter(_.isAvailable(houseState)).flatMap { card =>
        card.inputSpec match {
          case None => List(Command(playerId, card, None))
          case Some(SelectOwnerSlot) =>
            emptySlots.map { num =>
              Command(playerId, card, Some(new SlotInput(num)))
            }
          case Some(SelectOwnerCreature) =>
            slots.keys.map { num =>
              Command(playerId, card, Some(new SlotInput(num)))
            }
          case Some(SelectTargetCreature) =>
            otherSlots.keys.map { num =>
              Command(playerId, card, Some(new SlotInput(num)))
            }
        }
      }
    }(collection.breakOut)
  }
}
