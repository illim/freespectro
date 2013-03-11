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

  def simulateCommand(state: GameState, command: Command) : GameState = {
    simulateCommand(state, command.player, Some(command))
  }

  // todo return if someone died instead of reading state!
  def simulateCommand(state: GameState, playerId : PlayerId, commandOption: Option[Command]) : GameState = {
    val commandState = commandOption match {
      case None => state
      case Some(command) =>
        def applyEffect(st : GameState) =
          (st /: gameCard.getCommandEffect(command)) { (acc, f) =>
            f.exec(acc)
          }
        gameCard.debitAndSpawn(command).exec(applyEffect(state))
    }

    val runState = (commandState /: commandState.players(playerId).slots) {
      case (st, (numSlot, slot)) =>
        game.runSlot(playerId, numSlot, slot).exec(st)
    }
    game.prepareNextTurn(other(playerId)).exec(runState)
  }
}

case class RandomChoiceParam(chooseExpensive : Boolean = false)

class Choices(bot : ExtBot, rndParams : RandomChoiceParam = RandomChoiceParam()) {

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

  import util.Random

  def getRandomMove(state: GameState, playerId: PlayerId): Option[Command] = {
    val slots = state.players(playerId).slots
    val slot = Random.shuffle(slots).headOption
    val emptySlot = Random.shuffle(slotRange.filter(num => !slots.isDefinedAt(num))).headOption
    val otherSlot = Random.shuffle(state.players(other(playerId)).slots).headOption

    val houseDesc = bot.rippedDesc.players(playerId).houses(Random.nextInt(5))
    val houseState = state.players(playerId).houses(houseDesc.index)
    val cards = houseDesc.cardList.filter(_.isAvailable(houseState))

    val cardOption = if (rndParams.chooseExpensive) {
      if (cards.isEmpty) None
      else Some(cards.maxBy(_.cost))
    } else Random.shuffle(cards).headOption

    cardOption.flatMap { card =>
      card.inputSpec match {
        case None => Some(Command(playerId, card, None))
        case Some(SelectOwnerSlot) =>
          emptySlot.map { num =>
            Command(playerId, card, Some(new SlotInput(num)))
          }
        case Some(SelectOwnerCreature) =>
          slot.map { case (num, _) =>
            Command(playerId, card, Some(new SlotInput(num)))
          }
        case Some(SelectTargetCreature) =>
          otherSlot.map { case (num, _) =>
            Command(playerId, card, Some(new SlotInput(num)))
          }
      }
    }
  }
}
