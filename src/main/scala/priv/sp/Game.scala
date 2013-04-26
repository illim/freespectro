package priv.sp

import collection._
import priv._
import priv.sp.gui._
import scalaz._
import priv.sp.bot._
import java.util.concurrent._
import util.Utils._

class Game(val world: World, resources : GameResources, val server : GameServer) {
  import resources.updateExecutor

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
      world.addTask(new slotButton.FocusAnimTask())
    }
    def move(num : Int, dest : Int, playerId : PlayerId){
      val slotButton = slotPanels(playerId).slots(num)
      updateExecutor.waitLock{ lock =>
        world.addTask(new slotButton.MoveAnimTask(dest, lock))
      }
    }
  }))

  skipButton.on{ case MouseClicked(_) => commandRecorder.skip() }
  world.spawn(board.panel)
  world.spawn(Translate(Coord2i(0, 20), Column(List(surrenderButton, skipButton, settingsButton))))

  resources.updateExecutor.execute(waitPlayer(owner))

  def refresh(silent : Boolean = false) = {
    updateExecutor.waitLock{ lock =>
      world.addTask(new BlockingTask(board.refresh(silent), lock))
    }
  }

  protected def waitPlayer(player: PlayerId) {
    if (player == server.playerId) {
      cardPanels(player).setEnabled(true)
      updateExecutor.waitFor[Option[Command]]{ c =>
        server.waitNextCommand(c, state)
      }.foreach{ nextCommand =>
        submit(nextCommand, player)
      }
    } else {
      updateExecutor.waitFor[Option[Command]]{ c =>
        commandRecorder.startWith(c) {
          cardPanels(player).setEnabled(true)
        }
      }.foreach { nextCommand =>
        server.submitCommand(nextCommand)
        submit(nextCommand, player)
      }
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
    commandOption foreach { c =>
      if (c.card.isSpell){
        notifySpellPlayed(c.card)
        val sourceCoord = cardPanels(player).getPositionOf(c.card)
        val targetPlayer = if (c.input == Some(SelectOwnerCreature)) {
          player
        } else other(player)
        updateExecutor.waitLock{ lock =>
          world.doInRenderThread{
            slotPanels(targetPlayer).summonSpell(c, sourceCoord, lock)
          }
        }
      }
      gameUpdate.getCommandEffect(c).foreach(persist(_))
      refresh()
      persist(gameUpdate.debitAndSpawn(c))
      refresh(silent = true)
    }
    run(player)
  }

  protected def run(playerId: PlayerId) {
    state.checkEnded match {
      case Some(player) => endGame(player)
      case None =>
        println("run" + playerId)
        refresh()
        val player = state.players(playerId)
        val otherPlayerId = other(playerId)
        player.slots.toList.sortBy(_._1).foreach { case (numSlot, slot) =>
          if (slot.attack > 0 && slot.hasRunOnce){
            val slotButton = slotPanels(playerId).slots(numSlot)
            updateExecutor.waitLock{ lock =>
              world.addTask(new slotButton.RunAnimTask(lock))
            }
            val result = persist(gameUpdate.runSlot(playerId, numSlot, slot))
            refresh()
            state.checkEnded.foreach(endGame _)
          }
        }
        applySlotEffects(playerId, CardSpec.OnEndTurn)
        if (state.checkEnded.isEmpty){
          persist(gameUpdate.prepareNextTurn(otherPlayerId))
          applySlotEffects(otherPlayerId, CardSpec.OnTurn)
          refresh(silent = true)
          waitPlayer(otherPlayerId)
        }
    }
  }


  protected def applySlotEffects(playerId : PlayerId, phase : CardSpec.Phase, numSlot : Int = 0) {
    var ended = Option.empty[PlayerId]
    state.players(playerId).slots.get(numSlot) foreach { slotState =>
      gameUpdate.getSlotEffect(playerId, numSlot, slotState, phase) foreach { f =>
        val slotButton = slotPanels(playerId).slots(numSlot)
        persist(f)
        slotButton.focusAnim.foreach{ anim =>
          updateExecutor.waitLock{ lock =>
            world.addTask(Wait(anim.duration + anim.start - world.time, lock))
          }
        }
        refresh(silent = true)
        if (ended.isEmpty) ended = state.checkEnded
      }
    }
    ended match {
      case Some(playerId) => endGame(playerId)
      case None =>
        if (numSlot < nbSlots){
          applySlotEffects(playerId, phase, numSlot + 1)
        }
    }
  }

  private def notifySpellPlayed(card : Card) {
    world.doInRenderThread{
      import org.lwjgl.opengl.GL11._
      val cardTex = sp.textures.get("Images/Cards/" + card.image)
      world.addTask(TaskSpawn(world, duration = 1000L){
        glColor4f(1, 1, 1, 1)
        tex.drawAt(Coord2i(200, 100), cardTex.id, cardTex.size)
      })
    }
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
