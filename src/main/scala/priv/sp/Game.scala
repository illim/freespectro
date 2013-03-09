
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
  var state : GameState = GameState(List(PlayerState(p1State), PlayerState(p2State)))
  val desc = GameDesc(Array(p1Desc, p2Desc))
  val playersLs = playerIds.map(GameState.playerLens(_))
  private val bot = new MMBot(opponent, this)

  // gui
  val commandRecorder = new CommandRecorder(this)
  val playerPanels = playerIds.map(new CardPanel(_, this))
  val slotPanels = playerIds.map(new SlotPanel(_, this))
  val topCardPanel = new TopCardPanel(playerIds(opponent), this)
  val board = new Board(slotPanels, playerPanels, topCardPanel, sp)
  val gameCard = new GameCard(desc, this)
  world.entities.add(board.panel)

  waitPlayer(owner)

  protected def waitPlayer(player: PlayerId) {
    reset {
      val k = if (player == opponent) {
        shift { k: (Option[Command] => Unit) =>
          util.Utils.threaded {
            playerPanels(player).setEnabled(true)
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
    slotPanels.foreach(_.disable())
    playerPanels.foreach(_.setEnabled(false))
    commandOption match {
      case Some(c) =>
        persist(gameCard.summon(c))
        board.refresh(silent = true)
        reset {
          if (c.card.isSpell){
            slotPanels(other(player)).summonSpell(c.card)
          } else {
            shiftUnit0[Int, Unit](0)
          }
          gameCard.getCommandEffect(c).foreach(persist(_))
          run(player)
        }
      case None => run(player)
    }
  }

  protected def getSlotTurnEffect(playerId : PlayerId) = {
    val player = state.players(playerId)
    player.slots.foldLeft(state){ case (acc, (numSlot, slotState)) =>
      slotState.card.spec.effects(CardSpec.OnTurn) match {
        case Some(f) =>
          val env = new GameCardEffect.Env(playerId, this)
          env.selected = numSlot
          f(env) exec acc
        case None => acc
      }
    }
  }

  protected def run(playerId: PlayerId) {
    println("run" + playerId)
    board.refresh()
    val player = state.players(playerId)
    val otherPlayerId = other(playerId)
    val tasks = player.slots.toList.sortBy(_._1).collect {
      case (numSlot, slot) if slot.attack > 0 && slot.hasRunOnce =>
        val slotButton = slotPanels(playerId).slots(numSlot)

        new slotButton.AnimTask({
          val result = persist(runSlot(playerId, numSlot, slot))
          board.refresh()
          result
        })
    }
    reset {
      Task.chain(world, tasks).foreach(_.foreach(endGame _))
      persist(prepareNextTurn(otherPlayerId))
      persistState(getSlotTurnEffect(otherPlayerId))
      board.refresh(silent = true)
      waitPlayer(otherPlayerId)
    }
  }

  def runSlot(playerId: PlayerId, numSlot: Int, slot: SlotState): State[GameState, Option[PlayerId]] = {
    val otherPlayerLs = playersLs(other(playerId))

    def damageLife = State.get[GameState].flatMap{ st =>
      (otherPlayerLs.life -= st.players(other(playerId)).guard(slot.attack)).map { life =>
        if (life <= 0) {
          Some(playerId)
        } else None
      }
    }

    if (slot.card.multipleTarget){
      damageLife.flatMap{ result =>
        SlotState.inflictCreatures(otherPlayerLs, Damage(slot.attack)).map( _ => result)
      }
    } else {
      otherPlayerLs.slots.flatMap { slots =>
        slots.get(numSlot) match {
          case None => damageLife
          case Some(oppositeSlot) =>
            SlotState.inflictCreature(
              otherPlayerLs, numSlot, Damage(slot.attack)).map( _ => None)
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

  def summon(command: Command): State[GameState, Unit] = {
    val playerLs = playersLs(command.player)
    val debitMana = playerLs.houses.%== { houses =>
      val house = houses(command.card.houseIndex)
      houses.updated(command.card.houseIndex, new HouseState(house.mana - command.card.cost))
    }
    val summonIfCreature =
      if (command.card.spec.summon) {
        command.input match {
          case Some(slotInput) => playerLs.slots.%==(_ + (slotInput.num -> SlotState.creature(command.card)))
          case _ => GameState.unit
        }
      } else GameState.unit

    debitMana.flatMap(_ => summonIfCreature)
  }

  def getCommandEffect(command : Command) : Option[State[GameState, Unit]] = {
    command.card.spec.effects(CardSpec.Direct) map { f =>
      val env = new GameCardEffect.Env(command.player, game)
      command.input foreach { slotInput => env.selected = slotInput.num }
      f(env)
    }
  }
}
