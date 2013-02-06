package priv.sp

import priv.sp.gui._
import util.continuations._

class Game {

  val spWorld = new SpWorld
  var state = GameState(CardShuffle())
  val bot = new DummyBot

  // gui
  val commandRecorder = new CommandRecorder(this)
  val slotPanel = new SlotPanel(this)
  val playerPanels = state.players.map(new CardPanel(_, this))
  val board = new Board(slotPanel, playerPanels, spWorld)
  val entities = Seq(board)

  waitPlayer(owner)

  def waitPlayer(player: PlayerId) {
    reset {
      if (player == opponent) {
        submit(bot.executeAI(state.players(player)), player)
      } else {        
        playerPanels(player).setEnabled(true)
        val k = commandRecorder.start()
        submit(k, player)
      }
    }
  }

  def submit(commandOption: Option[(Command, CardEffects)], player: PlayerId) = {
    slotPanel.disable()
    playerPanels.foreach(_.setEnabled(false))
    commandOption.foreach {
      case (command, effects) =>
        effects.effects.foreach {
          case Summoned(card, numSlot) => state.players(player).slots(numSlot) = card
          case _ =>
        }
    }
    run(player)
  }

  def run(player: PlayerId) {
    slotPanel.refresh()
    // todo
    waitPlayer(swapPlayer(player))
  }

  def playerView(player: PlayerId) = new StateView(state.players(player))
  def slotView(player: PlayerId, numSlot: Int) = playerView(player).map(_.slots.get(numSlot))
}



