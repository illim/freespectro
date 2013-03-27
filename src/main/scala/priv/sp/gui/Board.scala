package priv.sp.gui

import priv._
import org.lwjgl.opengl.GL11._
import priv.sp._
import scala.util.continuations._

class Board(playerId : PlayerId, slotPanels: List[SlotPanel], playerPanels: List[CardPanel], topCardPanel: TopCardPanel, val sp: SpWorld) {

  val panel = getPanel()

  def refresh(silent : Boolean = false) {
    slotPanels.foreach(_.refresh())
    playerPanels.foreach(_.refresh(silent))
    topCardPanel.refresh(silent)
  }

  private def getPanel() = {
    val opponentPanel = playerPanels(other(playerId)).panel

    Column(
      List(
        Translate(Coord2i(500, - opponentPanel.size.y), opponentPanel),
        Translate(Coord2i(500, 0),topCardPanel.panel),
        Translate(
          Coord2i(320, 0),
          Column(List(
            slotPanels(other(playerId)).panel,
            Translate(
              Coord2i(0, -30), slotPanels(playerId).panel)))),
        Translate(
          Coord2i(500, 0), playerPanels(playerId).panel)))
  }

}

class CommandRecorder(game: Game) {
  val contNoop = Function.const[Unit, Option[Command]]() _
  private var value = Option.empty[Command]
  private var cont = contNoop
  def setCommand(command: Command) {
    game.slotPanels.foreach(_.disable())
    value = Some(command)
    nextStep()
  }

  def startWith(f: => Unit) = shift { k: (Option[Command] => Unit) =>
    value = None
    cont = k
    f
  }

  def addInput(x: SlotInput) = {
    value.foreach { command =>
      setCommand(command.copy(input = Some(x)))
    }
  }

  def skip(){
    continue(None)
  }

  private def continue(c : Option[Command]) = {
    cont(c)
    cont = contNoop
  }

  private def nextStep() {
    value.foreach { command =>
      if (command.card.inputSpec.size == command.input.size) {
        continue(Some(command))
      } else {
        command.card.inputSpec.get match {
          case SelectOwnerSlot => game.slotPanels(game.myPlayerId).setSlotEnabled(empty = true)
          case SelectOwnerCreature => game.slotPanels(game.myPlayerId).setSlotEnabled(empty = false)
          case SelectTargetCreature => game.slotPanels(game.otherPlayerId).setSlotEnabled(empty = false)
        }
      }
    }
  }
}
