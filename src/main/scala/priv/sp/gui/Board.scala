package priv.sp.gui

import priv._
import priv.entity._
import org.lwjgl.opengl.GL11._
import priv.sp._
import scala.util.continuations._

class Board(slotPanel: SlotPanel, playerPanels: List[CardPanel], val spWorld: SpWorld) extends Entity {

  private val currentPlayerPanel = playerPanels(owner)

  val panel = getPanel(currentPlayerPanel) // todo be able to see opponent board

  def render(world: World) {
    panel.render(world)
  }

  private def getPanel(playerPanel: CardPanel) = {
    Column(
      List(
        Translate(
          Coord2i(350, 0),
          Column(List(
            Row(slotPanel.topSlots),
            Row(slotPanel.bottomSlots)))),
        Translate(
          Coord2i(500, 0), playerPanel.panel)))
  }

}

class CommandRecorder(game: Game) {
  private var value = Option.empty[Command]
  private var cont = { _ : Option[(Command, CardEffects)] => () }
  def setCommand(command: Command) {
    value = Some(command)
    nextStep()
  }

  def start() = shift { k: (Option[(Command, CardEffects)] => Unit) =>
    value = None
    cont = k
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
        val commandEffects = applyEffect(command).map(effects => command -> effects)
        cont(commandEffects)
      } else {
        command.card.inputSpecs.steps(command.inputs.size) match {
          case SelectOwnerSlot => game.slotPanel.enableOwnerSlots()
          case SelectOwnerCreature =>
          case SelectTargetCreature =>
        }
      }
    }
  }

  private def applyEffect(command: Command): Option[CardEffects] = {
    command.card.spec match {
      case Summon =>
        command.inputs.headOption.collect {
          case OwnerSlot(num) =>
            val cardState = CardState.creature(command.card)
            CardEffects(List(Summoned(cardState, num)))
        }
      // todo anim(how to find positions) and callback to change state
      case _ => None
    }
  }
}

class SlotPanel(game: Game) {
  val topSlots = (0 to 5).map(num => SlotButton(num, game.slotView(opponent, num), game.spWorld))
  val bottomSlots = (0 to 5).map(num => SlotButton(num, game.slotView(owner, num), game.spWorld))
  val allSlots = topSlots ++ bottomSlots
  topSlots.foreach { slotButton =>
    slotButton.on {
      case MouseClicked(_) if slotButton.enabled =>
        game.commandRecorder.addInput(TargetCreature(slotButton.num))
    }
  }
  bottomSlots.foreach { slotButton =>
    slotButton.on {
      case MouseClicked(_) if slotButton.enabled =>
        game.commandRecorder.addInput(OwnerSlot(slotButton.num))
    }
  }

  def enableOwnerSlots() {
    bottomSlots.foreach { slot => if (slot.isEmpty) slot.enabled = true }
  }
  def disable() { allSlots.foreach(_.enabled = false) }
  def refresh() { allSlots.foreach(_.refresh()) }
}

class CardPanel(player: PlayerState, game: Game) {
  private val houseCardButtons = player.houseCards.map { house =>
    house -> house.cardStates.map { cardState =>
      CardButton(cardState, game.spWorld)
    }
  }
  val cardButtons = houseCardButtons.flatMap(_._2)
  cardButtons.foreach { cardButton =>
    cardButton.on {
      case MouseClicked(_) if cardButton.enabled =>
        game.commandRecorder.setCommand(Command(owner, cardButton.cardState.card, Nil))
    }
  }

  val panel = Row(houseCardButtons.map {
    case (house, cardButons) =>
      Column(new HouseLabel(house.house, game.spWorld) :: cardButons)
  })

  def setEnabled(flag: Boolean) {
    cardButtons.foreach(_.enabled = flag)
  }
}
