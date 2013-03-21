package priv.sp.bot

import priv.sp._

class Knowledge(game : Game, botPlayerId : PlayerId, knownCards : Set[(Card, Int)], val otherPlayerDesc : PlayerDesc) {
  val desc = ripPlayerState.exec(game.desc)
  val gameCard = new GameCard(desc, game)

  private def ripPlayerState = GameDesc.playerLens(other(botPlayerId))%==( _ => otherPlayerDesc)
}

trait Bot {
  def game: Game
  def botPlayerId: PlayerId
  def executeAI(state: GameState): Option[Command]

  var knownCards = Set.empty[(Card, Int)]
  var k = generateK().get

  def updateKnowledge(command : Command, indexOfCardInHouse : Int) {
    import command._
    val c = (card, indexOfCardInHouse)
    if (!knownCards.contains(c)
        && k.otherPlayerDesc.houses(card.houseIndex).cards(indexOfCardInHouse) != card) {
      knownCards += c
      generateK(2).foreach{ k = _ }
    }
  }

  def generateK(timeLimit : Int = Int.MaxValue) = {
    println("generating AI fake player")
    val start = System.currentTimeMillis
    game.shuffle.createAIPlayer(botPlayerId, knownCards, timeLimit).map{ fakePlayerDesc =>
      println("generated k in " + (System.currentTimeMillis -start)+" ms")
      new Knowledge(game, botPlayerId, knownCards, fakePlayerDesc)
    }
  }

  def simulateCommand(state: GameState, command: Command) : GameState = {
    simulateCommand(state, command.player, Some(command))
  }

  def simulateCommand(state: GameState, playerId : PlayerId, commandOption: Option[Command]) : GameState = {
    val commandState = commandOption match {
      case None => state
      case Some(command) =>
        def applyEffect(st : GameState) =
          (st /: k.gameCard.getCommandEffect(command)) { (acc, f) =>
            f.exec(acc)
          }
        k.gameCard.debitAndSpawn(command).exec(applyEffect(state))
    }

    var runState = (commandState /: commandState.players(playerId).slots) {
      case (st, (numSlot, slot)) =>
        game.runSlot(playerId, numSlot, slot).exec(st)
    }
    runState = game.prepareNextTurn(other(playerId)) exec runState
    if (runState.checkEnded.isEmpty) {
      runState = applySlotTurnEffects(playerId) exec runState
    }
    runState
  }

  private def applySlotTurnEffects(playerId : PlayerId) = scalaz.State.modify[GameState]{ st =>
    var newState = st
    for(numSlot <- slotRange){
      newState.players(playerId).slots.get(numSlot) foreach { slotState =>
        game.getSlotTurnEffect(playerId, numSlot, slotState).foreach{ f =>
          newState = f exec newState
        }
      }
    }
    newState
  }
}

class Choices(bot : Bot) {

  def getNexts(state: GameState, playerId: PlayerId): Stream[Command] = {
    val slots = state.players(playerId).slots
    val emptySlots = slotRange.filter(num => !slots.isDefinedAt(num))
    val otherSlots = state.players(other(playerId)).slots

    bot.k.desc.players(playerId).houses.flatMap { houseDesc  =>
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
    val houseDesc = bot.k.desc.players(playerId).houses(Random.nextInt(5))
    val houseState = state.players(playerId).houses(houseDesc.index)
    val cards = houseDesc.cardList.filter(_.isAvailable(houseState))
    val cardOption = Random.shuffle(cards).headOption

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
