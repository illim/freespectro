package priv.sp.gui

import priv._
import org.lwjgl.opengl.GL11._
import priv.sp._
import scala.util.continuations._

class Board(slotPanels: List[SlotPanel], playerPanels: List[CardPanel], topCardPanel: TopCardPanel, val sp: SpWorld) {

  val panel = getPanel()

  def refresh(silent : Boolean = false) {
    slotPanels.foreach(_.refresh())
    playerPanels.foreach(_.refresh(silent))
    topCardPanel.refresh(silent)
  }

  private def getPanel() = {
    val opponentPanel = playerPanels(opponent).panel

    Column(
      List(
        Translate(Coord2i(500, - opponentPanel.size.y), opponentPanel),
        Translate(Coord2i(500, 0),topCardPanel.panel),
        Translate(
          Coord2i(320, 0),
          Column(List(
            slotPanels(opponent).panel,
            Translate(
              Coord2i(0, -30), slotPanels(owner).panel)))),
        Translate(
          Coord2i(500, 0), playerPanels(owner).panel)))
  }

}

class CommandRecorder(game: Game) {
  private var value = Option.empty[Command]
  private var cont = Function.const[Unit, Option[Command]]() _
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

  def nextStep() {
    value.foreach { command =>
      if (command.card.inputSpec.size == command.input.size) {
        cont(Some(command))
      } else {
        command.card.inputSpec.get match {
          case SelectOwnerSlot => game.slotPanels(owner).setSlotEnabled(empty = true)
          case SelectOwnerCreature => game.slotPanels(owner).setSlotEnabled(empty = false)
          case SelectTargetCreature => game.slotPanels(opponent).setSlotEnabled(empty = false)
        }
      }
    }
  }

}

class SurrenderButton extends GuiElem {
  val size = Coord2i(100, 40)
  def render(world: World) {
    Fonts.draw(0, 20, "Surrender", 'white)
  }
}
