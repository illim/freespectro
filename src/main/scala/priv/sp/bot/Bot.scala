package priv.sp.bot

import priv.sp._
import priv.sp.update._

class Knowledge(gameDesc : GameDesc, botPlayerId : PlayerId, knownCards : Set[(Card, Int)], val otherPlayerDesc : PlayerDesc) {
  val desc = ripPlayerState.exec(gameDesc)
/**  println("AI K :" + otherPlayerDesc.houses.map{ h =>
    h.house.name + "/" + h.cards.toList
  })*/
  // bs
  def ripDescReader(gs : GameState) = {
    GameState(gs.players.zipWithIndex.map{ case (p, i) =>
      p.copy(desc = new DescReader(desc.players(i), p.desc.descMods))
    })
  }
  private def ripPlayerState = GameDesc.playerLens(other(botPlayerId))%==( _ => otherPlayerDesc)
}

trait Bot {
  def gameDesc: GameDesc
  def spHouses : Houses
  def botPlayerId: PlayerId
  def executeAI(state: GameState): Option[Command]

  val guess = new CardGuess(gameDesc, spHouses)
  var knownCards = Set.empty[(Card, Int)]
  var k = generateK().get
  var updater : GameStateUpdater = null

  def updateKnowledge(command : Command, indexOfCardInHouse : Int) {
    import command._
    val c = (card, indexOfCardInHouse)
    if (!knownCards.contains(c)) {
      knownCards += c
      generateK(2).foreach{ k = _ }
    }
  }

  def generateK(timeLimit : Int = Int.MaxValue) = {
    println("generating AI fake player")
    val start = System.currentTimeMillis
    guess.createAIPlayer(botPlayerId, knownCards, timeLimit).map{ fakePlayerDesc =>
      println("generated k in " + (System.currentTimeMillis -start)+" ms")
      new Knowledge(gameDesc, botPlayerId, knownCards, fakePlayerDesc)
    }
  }

  def reset(){
    knownCards = Set.empty
    k = generateK().get
  }

  private def initGameUpdate(state : GameState){
    if (updater == null){
      updater = new GameStateUpdater(state, gameDesc)
    }
  }

  def simulateCommand(state: GameState, command: Command) : (GameState, Transition) = {
    simulateCommand(state, command.player, Some(command))
  }

  def simulateCommand(state: GameState, playerId : PlayerId, commandOption: Option[Command]) : (GameState, Transition)  = {
    initGameUpdate(state)
    try {
      updater.lift{ u =>
        val p = u.players(playerId)

        commandOption foreach { command =>
          p.submit(command)
        }
        p.popTransition getOrElse {
          p.runSlots()
          if (!u.ended) {
            p.applyEffects(CardSpec.OnEndTurn)
            p.slots.toggleRun()
            val otherPlayer = p.otherPlayer
            otherPlayer.prepareNextTurn()
            if (!u.ended) {
              otherPlayer.applyEffects(CardSpec.OnTurn)
            }
          }
          WaitPlayer(other(playerId))
        }
      } run state
    } catch { case t : Throwable =>
      println("Failed on " + commandOption + "/" + state)
      throw t
    }
  }
}

class Choices(bot : Bot) {

  def getNexts(state: GameState, playerId: PlayerId): Stream[Command] = {
    val p = state.players(playerId)
    val slots = p.slots
    val openSlots = PlayerState.openSlots(p)
    val otherp = state.players(other(playerId))
    val otherOpenSlots = PlayerState.openSlots(otherp)
    val otherSlots = otherp.slots

    p.desc.get.houses.flatMap { houseDesc  =>
      val houseState = p.houses(houseDesc.house.houseIndex)

      houseDesc.cards.withFilter(_.isAvailable(houseState)).flatMap { cardDesc =>
        import cardDesc.card
        card.inputSpec match {
          case None => List(Command(playerId, card, None, cardDesc.cost))
          case Some(SelectOwnerSlot) =>
            openSlots.map { num =>
              Command(playerId, card, Some(new SlotInput(num)), cardDesc.cost)
            }
          case Some(SelectOwnerCreature) =>
            slots.keys.map { num =>
              Command(playerId, card, Some(new SlotInput(num)), cardDesc.cost)
            }
          case Some(SelectTargetSlot) =>
            otherOpenSlots.map { num =>
              Command(playerId, card, Some(new SlotInput(num)), cardDesc.cost)
            }
          case Some(SelectTargetCreature) =>
            otherSlots.keys.map { num =>
              Command(playerId, card, Some(new SlotInput(num)), cardDesc.cost)
            }
        }
      }
    }(collection.breakOut)
  }

  import util.Random

  def getRandomMove(state: GameState, playerId: PlayerId): Option[Command] = {
    val p = state.players(playerId)
    val otherp = state.players(other(playerId))
    val houseDesc = p.desc.get.houses(Random.nextInt(5))
    val houseState = p.houses(houseDesc.house.houseIndex)
    val cards = houseDesc.cards.filter(_.isAvailable(houseState))
    val cardOption = Random.shuffle(cards).headOption

    cardOption.flatMap { cardDesc =>
      import cardDesc.card
      card.inputSpec match {
        case None => Some(Command(playerId, card, None, cardDesc.cost))
        case Some(SelectOwnerSlot) =>
          Random.shuffle(PlayerState.openSlots(p)).headOption.map { num =>
            Command(playerId, card, Some(new SlotInput(num)), cardDesc.cost)
          }
        case Some(SelectOwnerCreature) =>
          Random.shuffle(state.players(playerId).slots).headOption.map { case (num, _) =>
            Command(playerId, card, Some(new SlotInput(num)), cardDesc.cost)
          }
        case Some(SelectTargetSlot) =>
          Random.shuffle(PlayerState.openSlots(otherp)).headOption.map { num =>
            Command(playerId, card, Some(new SlotInput(num)), cardDesc.cost)
          }
        case Some(SelectTargetCreature) =>
          Random.shuffle(state.players(other(playerId)).slots).headOption.map { case (num, _) =>
            Command(playerId, card, Some(new SlotInput(num)), cardDesc.cost)
          }
      }
    }
  }
}
