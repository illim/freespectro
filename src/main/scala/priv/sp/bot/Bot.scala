package priv.sp.bot

import priv.sp._

trait Bot {

  def executeAI(state: GameState): Option[Command]
}

trait ExtBot {
  def game: Game
  def botPlayerId : PlayerId
  
  // stupid stuff, ai don't know what we have so we create a fake player
  // todo give him all possible cards
  protected val fakePlayer = game.shuffle.createOnePlayer(game.shuffle.filterHouses(game.state.players(botPlayerId))(game.spWorld.houses.list))
  protected val ripPlayerState = game.playersLs(other(botPlayerId)).replaceCards(fakePlayer.houses)
  
  def getCommandChoices(state: GameState, playerId: PlayerId): Stream[Command] = {
    val cards = state.players(playerId).houses.flatMap { house => house.cards.filter(_.isAvailable(house)) }
    cards.toStream.flatMap { card =>
      card.inputSpec match {
        case None => List(Command(playerId, card, None))
        case Some(SelectOwnerSlot) =>
          slotRange.filterNot(s => state.players(playerId).slots.exists(_._1 == s)).map { num =>
            Command(playerId, card, Some(SlotInput(num)))
          }
        case Some(SelectOwnerCreature) => Nil
        case Some(SelectTargetCreature) => Nil
      }
    }
  }

  def simulateCommand(state: GameState, command: Command) = {
    val playerId = command.player
    val commandState = game.getCommandEffect(command).exec(state)
    val runState = (commandState /: commandState.players(playerId).slots) {
      case (st, (numSlot, slot)) =>
        game.runSlot(playerId, numSlot, slot).exec(st)
    }
    game.prepareNextTurn(other(playerId)).exec(runState)
  }
}