package priv.sp.gui

import priv._
import org.lwjgl.opengl.GL11._
import priv.sp._
import scala.util.continuations._
import priv.World
import priv.MouseClicked

class Board(slotPanel: SlotPanel, playerPanels: List[CardPanel], lifeLabels: List[LifeLabel], topCardPanel: TopCardPanel, val sp: SpWorld) extends Entity {

  private val currentPlayerPanel = playerPanels(owner)

  val panel = getPanel(currentPlayerPanel) // todo be able to see opponent board

  def render(world: World) {
    panel.render(world)
  }

  private def getPanel(playerPanel: CardPanel) = {
    Column(
      List(
        Translate(
          Coord2i(500, 0),
          topCardPanel.panel),
        Translate(
          Coord2i(320, 0),
          Column(List(
            Row(lifeLabels(opponent) :: slotPanel.slots(opponent)),
            Translate(
              Coord2i(0, -30), Row(lifeLabels(owner) :: /**TestButton(sp) :: */slotPanel.slots(owner)))))),
        Translate(
          Coord2i(500, 0), playerPanel.panel)))
  }

}

class CommandRecorder(game: Game) {
  private var value = Option.empty[Command]
  private var cont = Function.const[Unit, Option[Command]]() _
  def setCommand(command: Command) {
    value = Some(command)
    nextStep()
  }

  def startWith(f: => Unit) = shift { k: (Option[Command] => Unit) =>
    value = None
    cont = k
    f
  }

  def addInput(x: CardInput) = {
    value.foreach { command =>
      println("add input " + x)
      setCommand(command.copy(inputs = (x :: command.inputs)))
    }
  }

  def nextStep() {
    value.foreach { command =>
      if (command.card.inputSpecs.steps.size == command.inputs.size) {
        println("submit " + command.card)
        cont(Some(command))
      } else {
        command.card.inputSpecs.steps(command.inputs.size) match {
          case SelectOwnerSlot => game.slotPanel.enableOwnerSlots()
          case SelectOwnerCreature =>
          case SelectTargetCreature =>
        }
      }
    }
  }

}

class SlotPanel(game: Game) {
  val slots = game.playersLs.map { playerLs =>
    (0 to 5).map(num => new SlotButton(num, playerLs.slots.get(game.state).get(num), game.spWorld)).toList
  }
  val allSlots = slots.flatten
  slots(opponent).foreach { slotButton =>
    slotButton.on {
      case MouseClicked(_) if slotButton.enabled =>
        game.commandRecorder.addInput(TargetCreature(slotButton.num))
    }
  }
  slots(owner).foreach { slotButton =>
    slotButton.on {
      case MouseClicked(_) if slotButton.enabled =>
        game.commandRecorder.addInput(OwnerSlot(slotButton.num))
    }
  }

  def enableOwnerSlots() {
    slots(owner).foreach { slot => if (slot.isEmpty) slot.enabled = true }
  }
  def disable() { allSlots.foreach(_.enabled = false) }
  def refresh() { allSlots.foreach(_.refresh()) }
}

class CardPanel(playerLs : PlayerStateLenses, game: Game) {
  private val houseCardButtons = playerLs.houses.get(game.state).zipWithIndex.map { case (house, idx) =>
    def getHouse = playerLs.houses.get(game.state).apply(idx)
    
    new HouseLabel(getHouse, game.spWorld) -> house.cards.map { card =>
      new CardButton(card, getHouse, game.spWorld)
    }
  }
  val cardButtons = houseCardButtons.flatMap(_._2)
  cardButtons.foreach { cardButton =>
    cardButton.on {
      case MouseClicked(_) if cardButton.enabled =>
        game.commandRecorder.setCommand(Command(owner, cardButton.card, Nil))
    }
  }

  val panel = Row(houseCardButtons.map {
    case (houseLabel, cardButons) =>
      Column(houseLabel :: cardButons)
  })

  def setEnabled(flag: Boolean) {
    cardButtons.foreach(_.enabled = flag)
  }
}

class TopCardPanel(playerLs: PlayerStateLenses, game: Game) {
  val panel = Row(playerLs.houses.get(game.state).zipWithIndex.map { case (_, idx) =>
    new HouseLabel(playerLs.houses.get(game.state).apply(idx), game.spWorld, flip = true)
  })
}

