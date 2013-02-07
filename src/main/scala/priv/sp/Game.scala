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
  val board = new Board(slotPanel, playerPanels, spWorld)
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

  protected def run(player: PlayerId) {
    println("run" + player)
    slotPanel.refresh()
    val slots = state.players(player).slots
    val tasks = slots.collect {
      case (numSlot, slot) if slot.attack > 0 && slot.hasRunOnce =>
        val slotButton = slotPanel.slots(player)(numSlot)
        new slotButton.AnimTask(if (player == owner) -1 else 1)
    }
    reset {
      val k = world.chain(tasks)
      k
      slots.foreach{ case (_, slot) => if (! slot.hasRunOnce) slot.hasRunOnce = true}
      waitPlayer(swapPlayer(player))
    }
  }

}


