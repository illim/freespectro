
package priv.sp

import collection._
import priv._
import priv.sp.gui._
import scala.util.continuations._
import scalaz._

class Game(val world: World)
  extends SummonPhase
  with RunPhase {

  val spWorld = new SpWorld
  var state = GameState(CardShuffle())
  val playersLs = playerIds.map(GameState.playerLens(_))
  private val bot = new DummyBot

  // gui
  val commandRecorder = new CommandRecorder(this)
  val slotPanel = new SlotPanel(this)
  val playerPanels = playersLs.map(new CardPanel(_, this))
  val topCardPanel = new TopCardPanel(playersLs(opponent), this)
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

  protected def persist(stateFunc : State[GameState, _]) = {
    val (newState, _) = stateFunc run state
    state = newState
    stateFunc
  }
}


trait SummonPhase { _: Game =>

  protected def submit(commandOption: Option[Command], player: PlayerId) = {
    slotPanel.disable()
    playerPanels.foreach(_.setEnabled(false))
    commandOption.foreach(c => persist(getCommandEffect(c)))
    // todo anim
    println("submitted" + commandOption)
    run(player)
  }

  private def getCommandEffect(command: Command): State[GameState, Unit] = {
    val playerLs = playersLs(command.player)

    playerLs.houses.%== { houses =>
      val index = houses.indexWhere(_.cards.exists(_ == command.card))
      val house = houses(index)
      houses.updated(index, HouseState.manaL.mod(_ - command.card.cost, house))
    }.flatMap { _ =>
      def noop = playerLs.slots %== identity

      command.card.spec match {
        case Summon =>
          command.inputs.headOption match {
            case Some(OwnerSlot(num)) => playerLs.slots.%==(_ + (num -> SlotState.creature(command.card)))
            case _ => noop
          }
        case _ => noop
      }
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
          persist(runSlot(playerId, numSlot, slot))
          slotPanel.refresh()
        }
    }
    reset {
      val k = Task.chain(world, tasks)
      k
      persist(playersLs(otherPlayerId).slots.%==(_.map{ case (i, slot) => i -> SlotState.toggleRunOnce(slot) }).flatMap { _ =>
        playersLs(otherPlayerId).houses.%== (_.map(house => HouseState.manaL.mod(_ + 1, house)))
      })
      waitPlayer(otherPlayerId)
    }
  }

  def runSlot(playerId: PlayerId, numSlot: Int, slot: SlotState): State[GameState, Unit] = {
    val otherPlayerLs = playersLs(other(playerId))

    otherPlayerLs.slots.flatMap { slots =>
      slots.get(numSlot) match {
        case None =>
          (otherPlayerLs.life -= slot.attack).map { life =>
            if (life <= 0) {
              endGame(playerId)
            }
          }
        case Some(oppositeSlot) =>
          otherPlayerLs.slots.%= { slots =>
            slots + (numSlot -> SlotState.lifeL.mod(_ - slot.attack, oppositeSlot))
          }.flatMap { slots =>
            if (slots(numSlot).life <= 0) {
              otherPlayerLs.slots %== (_ - numSlot)
            } else {
              otherPlayerLs.slots %== (identity) // TODO refactor
            }
          }
      }
    }
  }
}


