
package priv.sp

import collection._
import priv._
import priv.sp.gui._
import scala.util.continuations._
import scalaz._
import priv.sp.bot._

class Game(val world: World) {

  val sp = new SpWorld
  val shuffle = new CardShuffle(sp)
  val List((p1Desc, p1State), (p2Desc, p2State)) = shuffle.get()
  var state = GameState(List(PlayerState(p1State), PlayerState(p2State)))
  val desc = GameDesc(Array(p1Desc, p2Desc))
  val playersLs = playerIds.map(GameState.playerLens(_))
  private val bot = new MMBot(opponent, this)

  // gui
  val commandRecorder = new CommandRecorder(this)
  val slotPanel = new SlotPanel(this)
  val playerPanels = playerIds.map(new CardPanel(_, this))
  val lifeLabels = playersLs.map(playerLs => new LifeLabel(playerLs.life.get(state)))
  val topCardPanel = new TopCardPanel(playerIds(opponent), this)
  val board = new Board(slotPanel, playerPanels, lifeLabels, topCardPanel, sp)
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

  protected def persistState(newState : GameState) {
    state = newState
  }

  protected def submit(commandOption: Option[Command], player: PlayerId) = {
    println(player + " submit " + commandOption)
    slotPanel.disable()
    playerPanels.foreach(_.setEnabled(false))
    commandOption.foreach(c => persist(gameCard.getCommandEffect(c)))
    // todo anim
    run(player)
  }

  protected def getSlotTurnEffect(playerId : PlayerId) = {
    val player = state.players(playerId)
    val env = new GameCardEffect.Env(playerId, this)
    player.slots.values.foldLeft(state){ (acc, slotState) =>
      slotState.card.spec.effects(CardSpec.OnTurn) match {
        case Some(f) => f(env) exec acc
        case None => acc
      }
    }
  }

  protected def run(playerId: PlayerId) {
    println("run" + playerId)
    slotPanel.refresh()

    val player = state.players(playerId)
    val otherPlayerId = other(playerId)
    val tasks = player.slots.toList.sortBy(_._1).collect {
      case (numSlot, slot) if slot.attack > 0 && slot.hasRunOnce =>
        val slotButton = slotPanel.slots(playerId)(numSlot)

        new slotButton.AnimTask({
          val result = persist(runSlot(playerId, numSlot, slot))
          slotPanel.refresh()
          result
        })
    }
    reset {
      val k = Task.chain(world, tasks)
      val result = k
      result.foreach(_.foreach(endGame _))
      persist(prepareNextTurn(otherPlayerId))
      persistState(getSlotTurnEffect(otherPlayerId))
      slotPanel.refresh()
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
        CardSpec.inflictCreatures(otherPlayerLs, slot.attack).map( _ => result)
      }
    } else {
      otherPlayerLs.slots.flatMap { slots =>
        slots.get(numSlot) match {
          case None => damageLife
          case Some(oppositeSlot) =>
            CardSpec.inflictCreature(
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

    val stateRun = debitMana.flatMap(_ => summonIfCreature)
    command.card.spec.effects(CardSpec.Direct) match {
      case None => stateRun
      case Some(f) =>
        val env = new GameCardEffect.Env(command.player, game)
        command.input foreach { slotInput => env.selected = slotInput.num }
        stateRun.flatMap( _ => f(env))
    }
  }
}
