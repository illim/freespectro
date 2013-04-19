package priv.sp

import collection._
import priv._
import priv.sp.gui._
import scala.util.continuations._
import scalaz._
import priv.sp.bot._
import java.util.concurrent._
import util.Utils.runnable

class Game(val world: World, resources : GameResources, val server : GameServer) {
  val sp = resources.sp
  var state : GameState = server.initState
  val desc = server.desc
  val myPlayerId = other(server.playerId)
  val otherPlayerId = server.playerId
  val names = playerIds.map{id => if (id == myPlayerId) "me" else server.name }

  // gui
  val commandRecorder = new CommandRecorder(this)
  val descriptionPanel = new DescriptionPanel(this)
  val cardPanels = playerIds.map(new CardPanel(_, this))
  val slotPanels = playerIds.map(new SlotPanel(_, this))
  val topCardPanel = new TopCardPanel(playerIds(otherPlayerId), this)
  val board = new Board(myPlayerId, slotPanels, cardPanels, topCardPanel, descriptionPanel, sp)
  val gameCard = new GameCard(desc)

  val surrenderButton = new GuiButton("Surrender")
  val skipButton = new GuiButton("Skip turn")
  val settingsButton = new GuiButton("Settings")

  skipButton.on{ case MouseClicked(_) => commandRecorder.skip() }
  world.spawn(board.panel)
  world.spawn(Translate(Coord2i(0, 20), Column(List(surrenderButton, skipButton, settingsButton))))

  waitPlayer(owner)

  protected def waitPlayer(player: PlayerId) {
    reset {
      val k = if (player == server.playerId) {
        shift { k: (Option[Command] => Unit) =>
          cardPanels(player).setEnabled(true)
          server.waitNextCommand(k, state)
        }
      } else {
        val commandOption = commandRecorder.startWith {
          cardPanels(player).setEnabled(true)
        }
        server.submitCommand(commandOption)
        commandOption
      }
      world.doInRenderThread(submit(k, player))
    }
  }

  protected def endGame(player: PlayerId) {
    val msg = if (player == myPlayerId) "YOU WON" else (names(player) + " WON")
    world.spawn(Translate(Coord2i(300, 350), new GuiButton(msg, Fonts.big)))
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
    cardPanels.foreach(_.setEnabled(false))
    commandOption match {
      case Some(c) =>
        reset {
          if (c.card.isSpell){
            notifySpellPlayed(c.card)
            val sourceCoord = cardPanels(player).getPositionOf(c.card)
            val targetPlayer = if (c.input == Some(SelectOwnerCreature)) {
              player
            } else other(player)
            slotPanels(targetPlayer).summonSpell(c, sourceCoord)
          } else {
            shiftUnit0[Int, Unit](0)
          }
          gameCard.getCommandEffect(c, state).foreach(persist(_))
          board.refresh()
          persist(gameCard.debitAndSpawn(c))
          board.refresh(silent = true)
          run(player)
        }
      case None => run(player)
    }
  }

  protected def run(playerId: PlayerId) {
    state.checkEnded match {
      case Some(player) => endGame(player)
      case None =>
        println("run" + playerId)
        board.refresh()
        val player = state.players(playerId)
        val otherPlayerId = other(playerId)
        val tasks = player.slots.toList.sortBy(_._1).collect {
          case (numSlot, slot) if slot.attack > 0 && slot.hasRunOnce =>
            val slotButton = slotPanels(playerId).slots(numSlot)

          new slotButton.RunAnimTask({
            val result = persist(Game.runSlot(playerId, numSlot, slot))
            board.refresh()
            state.checkEnded.foreach(endGame _)
            result
          })
        }
        reset {
          Task.chain(world, tasks)
          if (state.checkEnded.isEmpty){
            persist(Game.prepareNextTurn(otherPlayerId))
            applySlotTurnEffects(otherPlayerId)
            board.refresh(silent = true)
            waitPlayer(otherPlayerId)
          }
        }
    }
  }


  protected def applySlotTurnEffects(playerId : PlayerId, numSlot : Int = 0) {
    val tasks = state.players(playerId).slots.get(numSlot) flatMap { slotState =>
      Game.getSlotTurnEffect(playerId, numSlot, slotState, state).flatMap { f =>
        def applyEffect() = {
          persist(f)
          board.refresh(silent = true)
          state.checkEnded
        }
        if (!slotState.card.isFocusable){
          applyEffect()
          None
        } else {
          val slotButton = slotPanels(playerId).slots(numSlot)
          Some(new slotButton.FocusAnimTask(applyEffect()))
        }
      }
    }
    reset {
      Task.chain(world, tasks).foreach(_.foreach(endGame _))
      if (numSlot < nbSlots){
        applySlotTurnEffects(playerId, numSlot + 1)
      }
    }
  }

  private def notifySpellPlayed(card : Card) {
    import org.lwjgl.opengl.GL11._
    val cardTex = sp.textures.get("Images/Cards/" + card.image)
    world.addTask(TaskSpawn(world, duration = 1000L){
      glColor4f(1, 1, 1, 1)
      tex.drawAt(Coord2i(200, 100), cardTex.id, cardTex.size)
    })
  }
}


object Game {

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

  def getSlotTurnEffect(playerId : PlayerId, numSlot : Int, slotState : SlotState, snapshot : GameState) = {
    slotState.card.spec.effects(CardSpec.OnTurn) map { f =>
      val env = new GameCardEffect.Env(playerId, snapshot)
      env.selected = numSlot
      f(env)
    }
  }

}


// separated from game for ai to hack the description
class GameCard(desc : GameDesc) {

  def debitAndSpawn(command: Command): State[GameState, Unit] = {
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

  def getCommandEffect(command : Command, state : GameState) : Option[State[GameState, Unit]] = {
    command.card.spec.effects(CardSpec.Direct) map { f =>
      val env = new GameCardEffect.Env(command.player, state)
      command.input foreach { slotInput => env.selected = slotInput.num }
      f(env)
    }
  }
}
