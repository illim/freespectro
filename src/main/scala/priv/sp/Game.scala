package priv.sp

import priv._
import priv.sp.gui._
import scala.util.continuations._
import priv.util.StateView

class Game(val world: World)
  extends SummonPhase
  with RunPhase {

  val spWorld = new SpWorld
  var state = GameState(CardShuffle())
  private val bot = new DummyBot

  // gui
  val commandRecorder = new CommandRecorder(this)
  val slotPanel = new SlotPanel(this)
  val playerPanels = state.players.map(new CardPanel(_, this))
  val topCardPanel = new TopCardPanel(state.players(opponent), this)
  val board = new Board(slotPanel, playerPanels, topCardPanel, spWorld)
  world.entities.add(board)

  waitPlayer(owner)

  protected def waitPlayer(player: PlayerId) {
    if (player == opponent) {
      reset0(submit(bot.executeAI(state.players(player)), player))
    } else {
      reset {
        val k = commandRecorder.startWith {
          playerPanels(player).setEnabled(true)
        }
        submit(k, player)
      }
    }
  }

  protected def endGame(player: PlayerId) {
    println("winner : " + player)
    world.ended = true
  }

  def playerView(player: PlayerId) = new StateView(state.players(player))
  def slotView(player: PlayerId, numSlot: Int) = playerView(player).map(_.slots.get(numSlot))
}

trait SummonPhase { _: Game =>

  protected def submit(commandOption: Option[Command], player: PlayerId) = {
    slotPanel.disable()
    playerPanels.foreach(_.setEnabled(false))
    commandOption.foreach(applyEffect)
    println("submitted" + commandOption)
    run(player)
  }

  private def applyEffect(command: Command) = {
    state.players(command.player).houseCards.find(_.cardStates.exists(_.card == command.card)).foreach { houseCard =>
      houseCard.house.mana -= command.card.cost
    }
    command.card.spec match {
      case Summon =>
        command.inputs.headOption.collect {
          case OwnerSlot(num) =>
            state.players(command.player).slots += num -> CardState.creature(command.card)
        }
      // todo anim(how to find positions) and callback to change state
      case _ => None
    }
  }

}

trait RunPhase { _: Game =>

  protected def run(playerId: PlayerId) {
    println("run" + playerId)
    slotPanel.refresh()
    val player = state.players(playerId)
    val otherPlayerId = other(playerId)
    val otherPlayer = state.players(otherPlayerId)
    val tasks = player.slots.collect {
      case (numSlot, slot) if slot.attack > 0 && slot.hasRunOnce =>
        val slotButton = slotPanel.slots(playerId)(numSlot)

        slotButton.AnimTask(if (playerId == owner) -1 else 1) {
          otherPlayer.slots.get(numSlot) match {
            case None =>
              otherPlayer.life -= slot.attack
              if (otherPlayer.life <= 0) {
                endGame(playerId)
              }
            case Some(oppositeSlot) =>
              oppositeSlot.life -= slot.attack
              if (oppositeSlot.life <= 0) {
                otherPlayer.slots -= numSlot
                slotPanel.refresh()
              }
          }
        }
    }
    reset {
      val k = Task.chain(world, tasks)
      k
      player.slots.foreach(_._2.toggleRunOnce())
      state.players(otherPlayerId).houseCards.foreach { houseCard =>
        houseCard.house.mana += 1
      }
      waitPlayer(otherPlayerId)
    }
  }

}


