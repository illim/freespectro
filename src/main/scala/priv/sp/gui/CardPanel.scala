package priv.sp.gui

import priv._
import org.lwjgl.opengl.GL11._
import priv.sp._

class CardPanel(playerId: PlayerId, game: Game) {
  private val playerLs = playersLs(playerId)
  private val houseCardButtons = playerLs.houses.get(game.state).zipWithIndex.map { case (_, idx) =>
      def getHouseState = playerLs.houses.get(game.state).apply(idx)
      val house = game.desc.players(playerId).houses(idx)

      new HouseLabel(new DamagableInt(getHouseState.mana, game), house.house, game) -> house.cards.map { card =>
        new CardButton(card, getHouseState, game)
      }
  }
  val cardButtons = houseCardButtons.flatMap(_._2)
  val houseLabels = houseCardButtons.map(_._1)
  var lastSelected = Option.empty[CardButton]

  if (playerId == game.myPlayerId){
    cardButtons.foreach { cardButton =>
      cardButton.on {
        case MouseClicked(_) if cardButton.getIsActive =>
          import cardButton.card
          game.commandRecorder.setCommand(Command(game.myPlayerId, card, None))
          if (card.inputSpec.isDefined) {
            lastSelected.foreach(_.selected = false)
            cardButton.selected = true
            lastSelected = Some(cardButton)
          }
      }
    }
  }

  val panel = Row(houseCardButtons.map {
    case (houseLabel, cardButons) =>
      Column(houseLabel :: cardButons.toList)
  })
  setEnabled(false)

  def getPositionOf(card : Card) = {
    val cardButton = cardButtons.find(_.card == card).get
    cardButton.coord + (cardButton.size * 0.5)
  }
  def refresh(silent : Boolean) { houseLabels.foreach(_.mana.refresh(silent)) }
  def setEnabled(flag: Boolean) {
    cardButtons.foreach{ btn =>
      btn.enabled = flag
      lastSelected.foreach(_.selected = false)
      lastSelected = None
    }
  }
}

class TopCardPanel(playerId: PlayerId, game: Game) {
  private val playerLs = playersLs(playerId)
  val houseLabels = playerLs.houses.get(game.state).zipWithIndex.map {
    case (_, idx) =>
      new HouseLabel(new DamagableInt(playerLs.houses.get(game.state).apply(idx).mana, game), game.desc.players(playerId).houses(idx).house, game, flip = true)
  }
  val panel = Row(houseLabels)
  def refresh(silent : Boolean) { houseLabels.foreach(_.mana.refresh(silent)) }
}

