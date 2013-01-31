package priv.sp.gui

import priv._
import priv.entity._
import org.lwjgl.opengl.GL11._
import priv.sp._

class Board(gsm: GameStateMachine, val spWorld: SpWorld) extends Entity {
  private val slotPanel = new SlotPanel
  private val playerPanels = gsm.state.players.map(new CardPanel(_))
  private val currentPlayerPanel = playerPanels(owner)
  private val commandControler = new CommandControler
  val panel = getPanel(currentPlayerPanel) // todo be able to see opponent board

  gsm.onTransition {
    case _: Running =>
      slotPanel.disable()
      playerPanels.foreach(_.setEnabled(false))
    case Waiting(player) =>
      commandControler.reset()
      playerPanels(player).setEnabled(true)
  }

  def render(world: World) {
    panel.render(world)
  }

  def clean() {
    playerPanels.foreach(_.panel.clean())
  }

  private def getPanel(playerPanel: CardPanel) = {
    Column(
      List(
        Translate(
          Coord(350, 0),
          Column(List(
            Row(slotPanel.topSlots),
            Row(slotPanel.bottomSlots)))),
        Translate(
          Coord(500, 0), playerPanel.panel)))
  }


  class CardPanel(player: PlayerState) {
    private val houseCardButtons = player.houseCards.map { house =>
      house -> house.cardStates.map { cardState =>
        CardButton(cardState, spWorld)
      }
    }
    val cardButtons = houseCardButtons.flatMap(_._2)
    cardButtons.foreach { cardButton =>
      cardButton.on {
        case MouseClicked(_) =>
          commandControler.setCommand(Command(owner, cardButton.cardState.card, Nil))
      }
    }

    val panel = Row(houseCardButtons.map {
      case (house, cardButons) =>
        Column(new HouseLabel(house.house, spWorld) :: cardButons)
    })

    def setEnabled(flag: Boolean) {
      cardButtons.foreach(_.enabled = flag)
    }
  }
  
  
  class SlotPanel {
    val topSlots = (1 to 6).map(num => SlotButton(num, spWorld))
    val bottomSlots = (1 to 6).map(num => SlotButton(num, spWorld))
    val allSlots = topSlots ++ bottomSlots
    topSlots.foreach { slotButton =>
      slotButton.on {
        case MouseClicked(_) =>
          if (slotButton.enabled) {
            commandControler.addInput(TargetCreature(slotButton.num))
          }
      }
    }
    bottomSlots.foreach { slotButton =>
      slotButton.on {
        case MouseClicked(_) =>
          if (slotButton.enabled) {
            commandControler.addInput(OwnerSlot(slotButton.num))
          }
      }
    }
    
    def enableOwnerSlots(){
      bottomSlots.foreach(_.enabled = true)
    }
    def disable(){ allSlots.foreach(_.enabled = false)}
  }

  class CommandControler {
    private var value = Option.empty[Command]
    def setCommand(command: Command) {
      value = Some(command)
      submitIfReady()
    }
    def reset() { value = None }

    def addInput(x: CardInput) = {
      value.foreach { command =>
        println("add input "+x)
        setCommand(command.copy(inputs = (x :: command.inputs)))
      }
    }

    def submitIfReady() {
      value.foreach { command =>
        if (command.card.inputSpecs.steps.size == command.inputs.size) {
          println("submit " + command.card)
          gsm.goto(Running(Some(command), owner))
        } else {
          command.card.inputSpecs.steps(command.inputs.size) match {
            case SelectOwnerSlot => slotPanel.enableOwnerSlots()
            case SelectOwnerCreature =>
            case SelectTargetCreature =>
          }
        }
      }
    }
  }
}