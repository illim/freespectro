package priv.sp.gui

import priv._
import org.lwjgl.opengl.GL11._
import priv.sp._
import scala.util.continuations._

class Board(playerId : PlayerId, slotPanels: List[SlotPanel], cardPanels: List[CardPanel], topCardPanel: TopCardPanel, descriptionPanel : DescriptionPanel, val sp: SpWorld) {

  val panel = getPanel()

  def refresh(silent : Boolean = false) {
    slotPanels.foreach(_.refresh())
    cardPanels.foreach(_.refresh(silent))
    topCardPanel.refresh(silent)
  }

  private def getPanel() = {
    val opponentPanel = cardPanels(other(playerId)).panel

    Column(
      List(
        Translate(Coord2i(500, - opponentPanel.size.y), opponentPanel),
        Translate(Coord2i(500, 0), topCardPanel.panel),
        Translate(
          Coord2i(320, 0),
          Column(List(
            slotPanels(other(playerId)).panel,
            Translate(
              Coord2i(0, -30), slotPanels(playerId).panel)))),
        Row(List(
          Translate(Coord2i(50, 10), descriptionPanel),
          Translate(Coord2i(250, 0), cardPanels(playerId).panel)))))
  }

}

import priv.util.TVar

class CommandRecorder(game: Game) {
  private var value = Option.empty[Command]
  var cont  = Option.empty[TVar[Option[Command]]]

  def setCommand(command: Command) {
    game.slotPanels.foreach(_.disable())
    value = Some(command)
    nextStep()
  }

  def startWith(c : TVar[Option[Command]])(f: => Unit) {
    value = None
    cont = Some(c)
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
    cont.get.set(c)
    cont = None
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
