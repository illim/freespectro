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
              Coord2i(0, -30), Row(lifeLabels(owner) :: /**TestButton(sp) :: */ slotPanel.slots(owner)))))),
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
          case SelectOwnerSlot => game.slotPanel.setSlotEnabled(owner, empty = true)
          case SelectOwnerCreature => game.slotPanel.setSlotEnabled(owner, empty = false)
          case SelectTargetCreature => game.slotPanel.setSlotEnabled(opponent, empty = false)
        }
      }
    }
  }

}

class SlotPanel(game: Game) {
  val slots = game.playersLs.map { playerLs =>
    slotRange.map(num => new SlotButton(num, playerLs.slots.get(game.state).get(num), game.spWorld)).toList
  }
  val allSlots = slots.flatten
  allSlots.foreach(listenEvent)

  def listenEvent(slotButton: SlotButton) {
    slotButton.on {
      case MouseClicked(_) if slotButton.enabled =>
        game.commandRecorder.addInput(new SlotInput(slotButton.num))
    }
  }
  def setSlotEnabled(player: PlayerId, empty: Boolean) {
    slots(player).foreach { slot => if (slot.isEmpty == empty) slot.enabled = true }
  }
  def disable() { allSlots.foreach(_.enabled = false) }
  def refresh() { allSlots.foreach(_.refresh()) }
}

class CardPanel(playerId: PlayerId, game: Game) {
  private val playerLs = game.playersLs(playerId)
  private val houseCardButtons = playerLs.houses.get(game.state).zipWithIndex.map { case (_, idx) =>
      def getHouseState = playerLs.houses.get(game.state).apply(idx)
      val house = game.desc.players(playerId).houses(idx)

      new HouseLabel(getHouseState, house.house, game.spWorld) -> house.cards.map { card =>
        new CardButton(card, getHouseState, game.spWorld)
      }
  }
  val cardButtons = houseCardButtons.flatMap(_._2)
  cardButtons.foreach { cardButton =>
    cardButton.on {
      case MouseClicked(_) if cardButton.enabled =>
        game.commandRecorder.setCommand(Command(owner, cardButton.card, None))
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

class TopCardPanel(playerId: PlayerId, game: Game) {
  private val playerLs = game.playersLs(playerId)
  val panel = Row(playerLs.houses.get(game.state).zipWithIndex.map {
    case (_, idx) =>
      new HouseLabel(playerLs.houses.get(game.state).apply(idx), game.desc.players(playerId).houses(idx).house, game.spWorld, flip = true)
  })
}

