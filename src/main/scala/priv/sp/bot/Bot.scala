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
  private val fakePlayer = game.shuffle.createOnePlayer(game.shuffle.filterHouses(game.desc.players(botPlayerId))(game.spWorld.houses.list))
  private val ripPlayerState = GameDesc.playerLens(other(botPlayerId))%==( desc => GameDesc.replaceCards(fakePlayer._1.houses)(desc)._1)
  private val rippedDesc = ripPlayerState.exec(game.desc)

  val gameCard = new GameCard(rippedDesc, game)

  def getCommandChoices(state: GameState, playerId: PlayerId): Stream[Command] = {
    rippedDesc.players(playerId).houses.toStream.flatMap { houseDesc  =>
      val houseState = state.players(playerId).houses(houseDesc.index)
      val house = houseDesc.house

      houseDesc.cards.filter(_.isAvailable(houseState)).flatMap { card =>
        card.inputSpec match {
          case None => List(Command(playerId, card, None))
          case Some(SelectOwnerSlot) =>
            val slots = state.players(playerId).slots
            slotRange.collect { case num if ! slots.isDefinedAt(num) =>
              Command(playerId, card, Some(new SlotInput(num)))
            }
          case Some(SelectOwnerCreature) =>
            state.players(playerId).slots.map { slot =>
              Command(playerId, card, Some(new SlotInput(slot._1)))
            }
          case Some(SelectTargetCreature) =>
            state.players(other(playerId)).slots.map { slot =>
              Command(playerId, card, Some(new SlotInput(slot._1)))
            }
        }
      }
    }
  }

  def simulateCommand(state: GameState, command: Command) = {
    val playerId = command.player
    val commandState = gameCard.getCommandEffect(command).exec(state)
    val runState = (commandState /: commandState.players(playerId).slots) {
      case (st, (numSlot, slot)) =>
        game.runSlot(playerId, numSlot, slot).exec(st)
    }
    game.prepareNextTurn(other(playerId)).exec(runState)
  }
}
