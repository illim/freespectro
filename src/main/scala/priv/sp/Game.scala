package priv.sp

import java.util.concurrent._
import collection._
import scalaz._
import priv._
import priv.sp.bot._
import priv.sp.gui._
import priv.sp.update._
import util.Utils._

class Game(val world: World, resources : GameResources, val server : GameServer) { game =>

  var state      = server.initState
  val sp         = resources.sp
  val desc       = server.desc
  val myPlayerId    = other(server.playerId)
  val otherPlayerId = server.playerId
  val names         = playerIds.map{id => if (id == myPlayerId) "me" else server.name }
  var gameLock      = new priv.util.RichLock

  // gui
  val commandRecorder  = new CommandRecorder(this)
  val descriptionPanel = new DescriptionPanel(this)
  val cardPanels       = playerIds.map(new CardPanel(_, this))
  val slotPanels       = playerIds.map(new SlotPanel(_, this))
  val topCardPanel     = new TopCardPanel(playerIds(otherPlayerId), this)
  val board            = new Board(myPlayerId, slotPanels, cardPanels, topCardPanel, descriptionPanel, sp)

  val surrenderButton = new GuiButton("New game")
  val skipButton      = new GuiButton("Skip turn")
  val settingsButton  = new GuiButton("Settings")
  val restartButton   = new GuiButton("Restart")

  private val updater = new GameStateUpdater(state, desc)

  updater.updateListener = new GameUpdateListener
  skipButton.on{ case MouseClicked(_) =>
    if (state.checkEnded.isEmpty && commandRecorder.cont.isDefined) {
      commandRecorder.skip()
    }
  }
  restartButton.on{ case MouseClicked(_) =>
    world.forEntity[EndMessage](world.unspawn(_))
    cardPanels.foreach(_.setEnabled(false))
    gameLock.release()
    gameLock = new priv.util.RichLock
    server.reset()
    resources.gameExecutor.submit(runnable{
      persistState(server.initState)
      start()
    })
  }
  world.spawn(board.panel)
  world.spawn(Translate(Coord2i(0, 20), Column(List(surrenderButton, restartButton, settingsButton, skipButton))))
  resources.gameExecutor.submit(runnable(start()))

  private def start(){
    server.resetSeed()
    persist(updater.lift{ u =>
      playerIds.foreach{ id =>
        u.players(id).applyEffects(CardSpec.OnStart)
      }
    })
    refresh()
    waitPlayer(server.startingPlayer)
  }

  private def waitPlayer(player: PlayerId) {
    def autoSkip[A] : Option[Option[A]] = if (state.players(player).isDisabled) Some(None) else None

    if (player == server.playerId) {
      (autoSkip[Option[Command]]
       orElse {
         cardPanels(player).setEnabled(true)
         gameLock.waitFor[Option[Option[Command]]]{ c =>
           server.waitNextCommand(c, state)
         }}).foreach{ nextCommandOpt : Option[Option[Command]] =>
           nextCommandOpt match {
             case None => endGame(myPlayerId)
             case Some(nextCommand) =>
               for{
                 c <- nextCommand
                 b <- cardPanels(player).cardButtons.find(_.card == Some(c.card))
               } b.visible = true
               submit(nextCommand, player)
           }
         }
    } else {
      (autoSkip[Command]
       orElse gameLock.waitFor[Option[Command]]{ c =>
         commandRecorder.startWith(c) {
           cardPanels(player).setEnabled(true)
         }
       }).foreach { nextCommand =>
         server.submitCommand(nextCommand)
         submit(nextCommand, player)
       }
    }
  }

  private def endGame(player: PlayerId) {
    val msg = if (player == myPlayerId) "YOU WON" else (names(player) + " WON")
    world.spawn(new EndMessage(msg))
  }

  private def persist[A](stateFunc: State[GameState, A]) : A = {
    val result = stateFunc run state
    state = result._1
    result._2
  }
  private def persistState(newState : GameState) { state = newState  }
  private def persistUpdater() = persistState(updater.result)  // crappy side effect

  private def submit(commandOption: Option[Command], player: PlayerId) = {
    println(player + " submit " + commandOption)
    slotPanels.foreach(_.disable())
    cardPanels.foreach(_.setEnabled(false))
    commandOption foreach { c =>
      persist(updater.lift(_.players(c.player).submit(c)))
      refresh()
    }
    if(state.players(player).transitions.isEmpty) {
      run(player)
    } else {
      val t = persist(updater.lift{ u =>
        u.players(player).popTransition.get
      })
      t match {
        case WaitPlayer(p) => waitPlayer(p)
      }
    }
  }

  private def run(playerId: PlayerId) {
    def endOr(f : => Unit){
      state.checkEnded match {
        case Some(player) =>
          refresh()
          endGame(player)
        case None => f
      }
    }

    persist(updater.lift{ u =>
      val p = u.players(playerId)

      endOr {
        println("run" + playerId)
        p.runSlots()
        persistUpdater()
        refresh()
        endOr {
          p.applyEffects(CardSpec.OnEndTurn)
          p.slots.toggleRun()
          persistUpdater()
          endOr {
            val otherPlayer = p.otherPlayer
            otherPlayer.prepareNextTurn()
            otherPlayer.applyEffects(CardSpec.OnTurn)
            persistUpdater()
            refresh(silent = true)
            endOr {
              waitPlayer(otherPlayer.id)
            }
          }
        }
      }
    })
  }

  private def spawn(entity : => TimedEntity, blocking : Boolean = false){
    val lockOption = if (blocking) Some(gameLock.lock) else None
    world.doInRenderThread{
      world.addTask(TaskSpawn(entity, lockOption))
    }
    if (blocking) gameLock.lockWait()
  }

  private def refresh(silent : Boolean = false) = {
    gameLock.waitLock{ lock =>
      world.addTask(new BlockingTask(board.refresh(silent), lock))
    }
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
    }
    def summon(num : Int, slot : SlotState, playerId : PlayerId){
      val sourceCoord = (cardPanels(playerId).getPositionOf(slot.card) orElse cardPanels(other(playerId)).getPositionOf(slot.card)).getOrElse(Coord2i(0, 0))
      val slotButton = slotPanels(playerId).slots(num)
      spawn(slotButton.summon(sourceCoord, slot), blocking = true)
      persistUpdater()
//      refresh(silent = true)
    }
    def die(num : Int, playerId : PlayerId){
/**      val slotButton = slotPanels(playerId).slots(num)
      spawn(new slotButton.Fade, blocking = true)*/
    }
    def refresh(silent : Boolean) = {
      persistUpdater()
      game.refresh(silent)
      state.checkEnded.foreach(endGame _)
    }
    def spellPlayed(c : Command){
      spawn(new SpellNotif(sp, c.card))
      val sourceCoord = cardPanels(c.player).getPositionOf(c.card).getOrElse(Coord2i(0, 0))
      val targetPlayer = if (c.card.inputSpec == Some(SelectOwnerCreature)) {
        c.player
      } else other(c.player)
      gameLock.waitLock{ lock =>
        world.doInRenderThread{
          slotPanels(targetPlayer).summonSpell(c, sourceCoord, lock)
        }
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

class EndMessage(msg : String) extends Translate(Coord2i(100, 150), new GuiButton(msg, Fonts.big))
