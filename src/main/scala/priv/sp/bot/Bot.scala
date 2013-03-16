package priv.sp.bot

import priv.sp._

class Knowledge(game : Game, botPlayerId : PlayerId, knownCards : Set[(Card, Int)]) {
  val desc = ripPlayerState.exec(game.desc)
  val otherPlayerDesc = desc.players(other(botPlayerId))
  val gameCard = new GameCard(desc, game)
  var dirty = false

  private def ripPlayerState = {
    // todo give him all possible cards
    val fakePlayerDesc = {
      val getCardRange = new CardModel.ExcludePlayerCards(game.desc.players(other(botPlayerId)))
      val cardModel = CardModel.build(game.sp.houses, getCardRange)
      knownCards.foreach{ case (card, index) =>
        cardModel.houses(card.houseIndex).cards(index).assign(card.cost)
      }
      new CardShuffler(cardModel).solve()
      cardModel.toPlayerHouseDesc(game.sp.houses)
    }
    GameDesc.playerLens(other(botPlayerId))%==( desc => fakePlayerDesc)
  }
}

trait Bot {
  def game: Game
  def botPlayerId: PlayerId
  def executeAI(state: GameState): Option[Command]

  var knownCards = Set.empty[(Card, Int)]
  var k = generateK()

  def updateKnowledge(command : Command) {
    import command._
    val c = (card, index)
    if (!knownCards.contains(c)
        && k.otherPlayerDesc.houses(card.houseIndex).cards(index) != card) {
      knownCards += c
      k.dirty = true
    }
  }

  def generateK() = {
    println("generating AI fake player")
    val start = System.currentTimeMillis
    val k = new Knowledge(game, botPlayerId, knownCards)
    println("generated k in " + (System.currentTimeMillis -start)+" ms")
    k
  }

  def refreshK(){
    if (k.dirty){
      k = generateK()
    }
  }

  def simulateCommand(state: GameState, command: Command) : GameState = {
    simulateCommand(state, command.player, Some(command))
  }

  // todo return if someone died instead of reading state!
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

    val runState = (commandState /: commandState.players(playerId).slots) {
      case (st, (numSlot, slot)) =>
        game.runSlot(playerId, numSlot, slot).exec(st)
    }
    game.prepareNextTurn(other(playerId)).exec(runState)
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
