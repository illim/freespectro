
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
  val List((p1Desc, p1State), (p2Desc, p2State)) = shuffle.get()
  var state = GameState(List(PlayerState(p1State), PlayerState(p2State)))
  val desc = GameDesc(List(p1Desc, p2Desc))
  val playersLs = playerIds.map(GameState.playerLens(_))
  private val bot = new MMBot(opponent, this)

  // gui
  val commandRecorder = new CommandRecorder(this)
  val slotPanel = new SlotPanel(this)
  val playerPanels = playerIds.map(new CardPanel(_, this))
  val lifeLabels = playersLs.map(playerLs => new LifeLabel(playerLs.life.get(state)))
  val topCardPanel = new TopCardPanel(playerIds(opponent), this)
  val board = new Board(slotPanel, playerPanels, lifeLabels, topCardPanel, spWorld)
  val gameCard = new GameCard(desc, this)
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

  protected def persist[A](stateFunc: State[GameState, A]) : A = {
    val result = stateFunc run state
    state = result._1
    result._2
  }

  protected def getSlotPhaseEffect(playerId : PlayerId, phase : CardSpec.Phase) = {
    val playerLs = playersLs(playerId)
    val env = new GameCardEffect.Env(playerId, this)
    playerLs.slots.flatMap{ slots =>
      slots.values.map{ slotState =>
        slotState.card.spec.effectByPhase(phase)(env)
      }.reduceLeft{ (x, y) => x flatMap (_ => y)}
    }

  }
}

trait SummonPhase { _: Game =>

  protected def submit(commandOption: Option[Command], player: PlayerId) = {
    println(player + " submit " + commandOption)
    slotPanel.disable()
    playerPanels.foreach(_.setEnabled(false))
    commandOption.foreach(c => persist(gameCard.getCommandEffect(c)))
    // todo anim
    run(player)
  }

}

trait RunPhase { _: Game =>

  protected def run(playerId: PlayerId) {
    println("run" + playerId)
    slotPanel.refresh()
    persist(getSlotPhaseEffect(playerId, CardSpec.OnTurn))

    val player = state.players(playerId)
    val otherPlayerId = other(playerId)
    val otherPlayer = state.players(otherPlayerId)
    val tasks = player.slots.toList.sortBy(_._1).collect {
      case (numSlot, slot) if slot.attack > 0 && slot.hasRunOnce =>
        val slotButton = slotPanel.slots(playerId)(numSlot)

        slotButton.AnimTask(if (playerId == owner) -1 else 1) {
          val result = persist(runSlot(playerId, numSlot, slot))
          slotPanel.refresh()
          result
        }
    }
    reset {
      val k = Task.chain(world, tasks)
      val result = k
      result.foreach(_.foreach(endGame _))
      persist(prepareNextTurn(otherPlayerId))
      waitPlayer(otherPlayerId)
    }
  }

  def runSlot(playerId: PlayerId, numSlot: Int, slot: SlotState): State[GameState, Option[PlayerId]] = {
    val otherPlayerLs = playersLs(other(playerId))

    def damageLife = (otherPlayerLs.life -= slot.attack).map { life =>
      if (life <= 0) {
        Some(playerId)
      } else None
    }

    if (slot.card.multipleTarget){
      damageLife.flatMap{ result =>
        GameCardEffect.damageCreatures(otherPlayerLs, slot.attack).map( _ => result)
      }
    } else {
      otherPlayerLs.slots.flatMap { slots =>
        slots.get(numSlot) match {
          case None => damageLife
          case Some(oppositeSlot) =>
            GameCardEffect.damageCreature(
              otherPlayerLs, numSlot, slot.attack).map( _ => None)
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


// separated from game for ai to hack the description
class GameCard(desc : GameDesc, game : Game) {
  import game._

  def getCommandEffect(command: Command): State[GameState, Unit] = {
    val playerLs = playersLs(command.player)
    val debitMana = playerLs.houses.%== { houses =>
      val house = houses(command.card.houseIndex)
      houses.updated(command.card.houseIndex, HouseState.manaL.mod(_ - command.card.cost, house))
    }
    val summonIfCreature =
      if (command.card.spec.summon) {
        command.input match {
          case Some(slotInput) => playerLs.slots.%==(_ + (slotInput.num -> SlotState.creature(command.card)))
          case _ => GameState.unit
        }
      } else GameState.unit

    val env = new GameCardEffect.Env(command.player, game)
    command.input foreach { slotInput => env.selected = slotInput.num }
    debitMana.flatMap(_ => summonIfCreature).flatMap(_ => command.card.spec.directEffects(env))
  }
}
