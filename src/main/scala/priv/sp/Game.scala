
package priv.sp

import collection._
import priv._
import priv.sp.gui._
import scala.util.continuations._
import scalaz._
import priv.sp.bot._

class Game(val world: World)
  extends SummonPhase
  with RunPhase {

  val spWorld = new SpWorld
  val shuffle = new CardShuffle(spWorld)
  var state = GameState(shuffle.get())
  val playersLs = playerIds.map(GameState.playerLens(_))
  private val bot = new DummyBot2(opponent, this)

  // gui
  val commandRecorder = new CommandRecorder(this)
  val slotPanel = new SlotPanel(this)
  val playerPanels = playersLs.map(new CardPanel(_, this))
  val lifeLabels = playersLs.map(playerLs => new LifeLabel(playerLs.life.get(state)))
  val topCardPanel = new TopCardPanel(playersLs(opponent), this)
  val board = new Board(slotPanel, playerPanels, lifeLabels, topCardPanel, spWorld)
  world.entities.add(board)

  waitPlayer(owner)

  protected def waitPlayer(player: PlayerId) {
    reset {
      val k = if (player == opponent) {
        shift { k: (Option[Command] => Unit) =>
          util.Utils.threaded {
            k(bot.executeAI(state))
          }
        }
      } else {
        commandRecorder.startWith {
          playerPanels(player).setEnabled(true)
        }
      }
      submit(k, player)
    }
  }

  protected def endGame(player: PlayerId) {
    println("winner : " + player)
    world.ended = true
  }

  protected def persist(stateFunc: State[GameState, _]) = {
    state = stateFunc.exec(state)
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

  def getCommandEffect(command: Command): State[GameState, Unit] = {
    val playerLs = playersLs(command.player)
    val debitMana = playerLs.houses.%== { houses =>
      val index = houses.indexWhere(_.cards.exists(_ == command.card))
      val house = houses(index)
      houses.updated(index, HouseState.manaL.mod(_ - command.card.cost, house))
    }
    val summonIfCreature =
      if (command.card.spec.summon) {
        command.input match {
          case Some(SlotInput(num)) => playerLs.slots.%==(_ + (num -> SlotState.creature(command.card)))
          case _ => GameState.unit
        }
      } else GameState.unit

    val env = new GameCardEffect.Env(command.player, this)
    command.input foreach { slotInput => env.selected = slotInput.num }
    val directEffects =
      command.card.spec.effects.foldLeft(GameState.unit) {
        case (acc, (CardSpec.Direct, effect)) => acc.flatMap(_ => GameCardEffect.getCardEffect(effect, env))
        case (acc, _) => acc
      }
    debitMana.flatMap(_ => summonIfCreature).flatMap(_ => directEffects)
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
      persist(prepareNextTurn(otherPlayerId))
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
              GameState.unit
            }
          }
      }
    }
  }

  def prepareNextTurn(playerId: PlayerId): State[GameState, Unit] = {
    playersLs(playerId).slotsToggleRun.flatMap { _ =>
      playersLs(playerId).housesIncrMana
    }
  }
}


