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
  var state      = server.initState
  val sp         = resources.sp
  val desc       = server.desc
  val myPlayerId    = other(server.playerId)
  val otherPlayerId = server.playerId
  val names         = playerIds.map{id => if (id == myPlayerId) "me" else server.name }

  // gui
  val commandRecorder  = new CommandRecorder(this)
  val descriptionPanel = new DescriptionPanel(this)
  val cardPanels       = playerIds.map(new CardPanel(_, this))
  val slotPanels       = playerIds.map(new SlotPanel(_, this))
  val topCardPanel     = new TopCardPanel(playerIds(otherPlayerId), this)
  val board            = new Board(myPlayerId, slotPanels, cardPanels, topCardPanel, descriptionPanel, sp)

  val surrenderButton = new GuiButton("Surrender")
  val skipButton      = new GuiButton("Skip turn")
  val settingsButton  = new GuiButton("Settings")

  private val gameUpdate = GameUpdate(new GameStateUpdater(state, new UpdateListener{
    def focus(num : Int, playerId : PlayerId){
      val slotButton = slotPanels(playerId).slots(num)
      world.addTask(new slotButton.FocusAnimTask)
    }
  }))

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
          gameUpdate.getCommandEffect(c).foreach(persist(_))
          board.refresh()
          persist(gameUpdate.debitAndSpawn(c))
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
            val result = persist(gameUpdate.runSlot(playerId, numSlot, slot))
            board.refresh()
            state.checkEnded.foreach(endGame _)
            result
          })
        }
        reset {
          Task.chain(world, tasks)
          applySlotEffects(playerId, CardSpec.OnEndTurn)
          if (state.checkEnded.isEmpty){
            persist(gameUpdate.prepareNextTurn(otherPlayerId))
            applySlotEffects(otherPlayerId, CardSpec.OnTurn)
            board.refresh(silent = true)
            waitPlayer(otherPlayerId)
          }
        }
    }
  }


  protected def applySlotEffects(playerId : PlayerId, phase : CardSpec.Phase, numSlot : Int = 0) {
    var ended = Option.empty[PlayerId]
    val tasks = state.players(playerId).slots.get(numSlot) flatMap { slotState =>
      gameUpdate.getSlotEffect(playerId, numSlot, slotState, phase).flatMap { f =>
        val slotButton = slotPanels(playerId).slots(numSlot)
        persist(f)
        board.refresh(silent = true)
        if (ended.isEmpty) ended = state.checkEnded
        slotButton.focusAnim.map{ anim =>
          Wait(anim.duration + anim.start - world.time) // crap
        }
      }
    }
    reset {
      Task.chain(world, tasks)
      ended.foreach(endGame _)
      if (numSlot < nbSlots && ended.isEmpty){
        applySlotEffects(playerId, phase, numSlot + 1)
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

object GameUpdate {
  def apply(x : GameStateUpdater) = {
    val gu = new GameUpdate
    gu.updater = x
    gu
  }
}
class GameUpdate {
  var updater : GameStateUpdater = null // horror to remove crappy dependency(todo create fake structure to init correctly the updater)

  def runSlot(playerId: PlayerId, numSlot: Int, slot: SlotState) = updater.lift { u =>
    val otherPlayerUpdate = u.players(other(playerId))
    val d = Damage(slot.attack)

    if (slot.card.multipleTarget){
      otherPlayerUpdate.slots.inflictMultiTarget(d)
    } else {
      otherPlayerUpdate.slots.value.get(numSlot) match {
        case None => otherPlayerUpdate.inflict(d)
        case Some(oppositeSlot) => otherPlayerUpdate.slots.inflictCreature(numSlot, d)
      }
    }
  }

  def prepareNextTurn(playerId: PlayerId) = updater.lift{ u =>
    val playerUpdate = u.players(playerId)
    playerUpdate.slots.toggleRun()
    playerUpdate.houses.incrMana()
  }

  def getSlotEffect(playerId : PlayerId, numSlot : Int, slotState : SlotState, phase : CardSpec.Phase) = {
    slotState.card.effects(phase) map { f =>
      updater.lift{ u =>
        val env = new GameCardEffect.Env(playerId, u)
        env.selected = numSlot
        f(env)
      }
    }
  }

  def debitAndSpawn(command: Command): State[GameState, Unit] = updater.lift{ u =>
    val playerUpdate = u.players(command.player)

    playerUpdate.houses.incrMana(- command.card.cost, command.card.houseIndex)
    if (!command.card.isSpell) {
      command.input.foreach{ slotInput =>
        playerUpdate.slots.summon(slotInput.num, SlotState.asCreature(command.card))
      }
    }
  }

  def getCommandEffect(command : Command) : Option[State[GameState, Unit]] = {
    command.card.effects(CardSpec.Direct) map { f =>
      updater.lift{ u =>
        val env = new GameCardEffect.Env(command.player, u)
        command.input foreach { slotInput => env.selected = slotInput.num }
        f(env)
      }
    }
  }
}
