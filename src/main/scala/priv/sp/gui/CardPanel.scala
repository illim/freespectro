package priv.sp.gui

import priv._
import org.lwjgl.opengl.GL11._
import priv.sp._

class CardPanel(playerId: PlayerId, game: Game) {
  private val houseCardButtons = game.desc.players(playerId).houses.zipWithIndex.map { case (houseDesc, idx) =>
      def getHouseState = game.state.players(playerId).houses(idx)

      new HouseLabel(new DamagableInt(getHouseState.mana, game), houseDesc.house, game) -> houseDesc.cards.map { card =>
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
  val houseLabels = game.desc.players(playerId).houses.zipWithIndex.map {
    case (houseDesc, idx) =>
      new HouseLabel(new DamagableInt(game.state.players(playerId).houses(idx).mana, game), houseDesc.house, game, flip = true)
  }
  val panel = Row(houseLabels)
  def refresh(silent : Boolean) { houseLabels.foreach(_.mana.refresh(silent)) }
}

