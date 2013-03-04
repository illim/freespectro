package priv.sp.gui

import priv._
import org.lwjgl.opengl.GL11._
import priv.sp._
import scala.util.continuations._

class Board(slotPanels: List[SlotPanel], playerPanels: List[CardPanel], topCardPanel: TopCardPanel, val sp: SpWorld) extends Entity {

  private val currentPlayerPanel = playerPanels(owner)

  val panel = getPanel(currentPlayerPanel) // todo be able to see opponent board

  def render(world: World) {
    panel.render(world)
  }

  def refresh(silent : Boolean = false) {
    slotPanels.foreach(_.refresh())
    playerPanels.foreach(_.refresh(silent))
    topCardPanel.refresh(silent)
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
            slotPanels(opponent),
            Translate(
              Coord2i(0, -30), slotPanels(owner))))),
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

class CardPanel(playerId: PlayerId, game: Game) {
  private val playerLs = game.playersLs(playerId)
  private val houseCardButtons = playerLs.houses.get(game.state).zipWithIndex.map { case (_, idx) =>
      def getHouseState = playerLs.houses.get(game.state).apply(idx)
      val house = game.desc.players(playerId).houses(idx)

      new HouseLabel(new DamagableInt(getHouseState.mana, game), house.house, game) -> house.cards.map { card =>
        new CardButton(card, getHouseState, game.sp)
      }
  }
  val cardButtons = houseCardButtons.flatMap(_._2)
  val houseLabels = houseCardButtons.map(_._1)
  cardButtons.foreach { cardButton =>
    cardButton.on {
      case MouseClicked(_) if cardButton.enabled =>
        game.commandRecorder.setCommand(Command(owner, cardButton.card, None))
    }
  }

  val panel = Row(houseCardButtons.map {
    case (houseLabel, cardButons) =>
      Column(houseLabel :: cardButons.toList)
  })

  def refresh(silent : Boolean) { houseLabels.foreach(_.mana.refresh(silent)) }

  def setEnabled(flag: Boolean) {
    cardButtons.foreach(_.enabled = flag)
  }
}

class TopCardPanel(playerId: PlayerId, game: Game) {
  private val playerLs = game.playersLs(playerId)
  val houseLabels = playerLs.houses.get(game.state).zipWithIndex.map {
    case (_, idx) =>
      new HouseLabel(new DamagableInt(playerLs.houses.get(game.state).apply(idx).mana, game), game.desc.players(playerId).houses(idx).house, game, flip = true)
  }
  val panel = Row(houseLabels)
  def refresh(silent : Boolean) { houseLabels.foreach(_.mana.refresh(silent)) }
}

