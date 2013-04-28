package priv.sp

import collection._
import priv._
import priv.sp.gui._
import scalaz._
import priv.sp.bot._
import java.util.concurrent._
import util.Utils._

class Game(val world: World, resources : GameResources, val server : GameServer) { game =>
  import resources.gameExecutor

  var state      = server.initState
  val sp         = resources.sp
  val desc       = server.desc
  val myPlayerId    = other(server.playerId)
  val otherPlayerId = server.playerId
  val names         = playerIds.map{id => if (id == myPlayerId) "me" else server.name }
  val gameLock      = new priv.util.RichLock

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

  private val gameUpdate = GameUpdate(new GameStateUpdater(state))

  gameUpdate.updater.updateListener = new GameUpdateListener
  skipButton.on{ case MouseClicked(_) => commandRecorder.skip() }
  world.spawn(board.panel)
  world.spawn(Translate(Coord2i(0, 20), Column(List(surrenderButton, skipButton, settingsButton))))
  resources.gameExecutor.submit(runnable(waitPlayer(owner)))

  def refresh(silent : Boolean = false) = {
    gameLock.waitLock{ lock =>
      world.addTask(new BlockingTask(board.refresh(silent), lock))
    }
  }

  protected def waitPlayer(player: PlayerId) {
    if (player == server.playerId) {
      cardPanels(player).setEnabled(true)
      gameLock.waitFor[Option[Command]]{ c =>
        server.waitNextCommand(c, state)
      }.foreach{ nextCommand =>
        submit(nextCommand, player)
      }
    } else {
      gameLock.waitFor[Option[Command]]{ c =>
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
        gameLock.waitLock{ lock =>
          world.doInRenderThread{
            slotPanels(targetPlayer).summonSpell(c, sourceCoord, lock)
          }
        }
      }
      persist(gameUpdate.submit(c))
    }
    run(player)
  }

  protected def run(playerId: PlayerId) {
    state.checkEnded match {
      case Some(player) => endGame(player)
      case None =>
        println("run" + playerId)
        refresh()
        persist(gameUpdate.runSlots(playerId))
        refresh()
        applySlotEffects(playerId, CardSpec.OnEndTurn)
        if (state.checkEnded.isEmpty){
          val otherPlayerId = other(playerId)
          persist(gameUpdate.prepareNextTurn(otherPlayerId))
          applySlotEffects(otherPlayerId, CardSpec.OnTurn)
          refresh(silent = true)
          waitPlayer(otherPlayerId)
        }
    }
  }

  // this is probably bugged due to card moves ...
  // todo identify slot creature?
  protected def applySlotEffects(playerId : PlayerId, phase : CardSpec.Phase) {
    ((Option.empty[PlayerId] /: state.players(playerId).slots) {
      case (None, (numSlot, slotState)) =>
        if (state.players(playerId).slots.isDefinedAt(numSlot)){
          gameUpdate.getSlotEffect(playerId, numSlot, slotState, phase) flatMap { f =>
            persist(f)
            refresh(silent = true)
            state.checkEnded
          }
        } else None
      case (acc, _) => acc
    }).foreach(endGame _)
  }

  private def notifySpellPlayed(card : Card) = spawn(new SpellNotif(sp, card))

  private def spawn(entity : => TimedEntity, blocking : Boolean = false){
    val lockOption = if (blocking) Some(gameLock.lock) else None
    world.doInRenderThread{
      world.addTask(TaskSpawn(entity, lockOption))
    }
    if (blocking) gameLock.lockWait()
  }

  private class GameUpdateListener extends UpdateListener {
    def focus(num : Int, playerId : PlayerId, blocking : Boolean){
      val slotButton = slotPanels(playerId).slots(num)
      spawn(new slotButton.Focus(), blocking)
    }
    def move(num : Int, dest : Int, playerId : PlayerId){
      val slotButton = slotPanels(playerId).slots(num)
      gameLock.waitLock{ lock =>
        world.addTask(new slotButton.MoveAnimTask(dest, lock))
      }
    }
    def runSlot(num : Int, playerId : PlayerId){
      val slotButton = slotPanels(playerId).slots(num)
      spawn(Running(slotButton.location, slotButton.direction), blocking = true)
      persistState(gameUpdate.updater.result) // crappy side effect
      refresh()
      state.checkEnded.foreach(endGame _)
    }
    def summon(num : Int, slot : SlotState, playerId : PlayerId){
      val sourceCoord = cardPanels(playerId).getPositionOf(slot.card)
      val slotButton = slotPanels(playerId).slots(num)
      spawn(slotButton.summon(sourceCoord, slot), blocking = true)
      persistState(gameUpdate.updater.result)
      refresh(silent = true)
    }
    def refresh(silent : Boolean) = game.refresh(silent)
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

  def submit(c : Command) = updater.lift(_.players(c.player).submit(c))

  def runSlots(playerId: PlayerId) = updater.lift{ u =>
    val playerUpdate = u.players(playerId)
    playerUpdate.runSlots()
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
}

class SpellNotif(sp : SpWorld, card: Card) extends TimedEntity {
  import org.lwjgl.opengl.GL11._
  val duration = 1000L
  val cardTex = sp.textures.get("Images/Cards/" + card.image)

  def render(){
    glColor4f(1, 1, 1, 1)
    tex.drawAt(Coord2i(200, 100), cardTex.id, cardTex.size)
  }
}
