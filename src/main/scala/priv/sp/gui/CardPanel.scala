package priv.sp.gui

import priv._
import org.lwjgl.opengl.GL11._
import priv.sp._

class CardPanel(playerId: PlayerId, game: Game) {
  private val houseCardButtons = game.desc.players(playerId).houses.zipWithIndex.map {
    case (houseDesc, idx) ⇒
      def getCard(i: Int) = {
        val cards = game.state.players(playerId).desc.get.houses(idx).cards
        if (i < cards.size) Some(cards(i)) else None
      }
      def getHouseState = game.state.players(playerId).houses(idx)
      new HouseLabel(new DamagableInt(getHouseState.mana, game), houseDesc.house, game) -> (0 to 3).map { i ⇒
        new CardButton(getCard(i), getHouseState, game)
      }
  }
  val cardButtons = houseCardButtons.flatMap(_._2)
  val houseLabels = houseCardButtons.map(_._1)
  var lastSelected = Option.empty[CardButton]

  if (playerId == game.myPlayerId) {
    cardButtons.foreach { cardButton ⇒
      cardButton.visible = true
      cardButton.on {
        case MouseClicked(_) if cardButton.isActive ⇒
          cardButton.holder.foreach { h ⇒
            game.commandRecorder.setCommand(Command(game.myPlayerId, h.desc.card, None, h.desc.cost))
            if (h.desc.card.inputSpec.isDefined) {
              lastSelected.foreach(_.selected = false)
              cardButton.selected = true
              lastSelected = Some(cardButton)
            }
          }
      }
    }
  }

  val panel = Row(houseCardButtons.map {
    case (houseLabel, cardButons) ⇒
      Column(houseLabel :: cardButons.toList)
  })
  setEnabled(false)

  var visibleCards = Set.empty[Card]
  def addVisibleCard(c: Card) {
    visibleCards = visibleCards + c
  }

  def getPositionOf(card: Card) = {
    val someCard = Some(card)
    cardButtons.find(_.card == someCard).map { cardButton ⇒
      cardButton.coord + (cardButton.size * 0.5)
    }
  }
  val specialCardButtons = houseCardButtons(4)._2
  def refresh(silent: Boolean) {
    houseLabels.foreach(_.mana.refresh(silent))
    cardButtons.foreach(_.refresh())
    cardButtons.foreach { cb ⇒
      cb.visible = playerId == game.myPlayerId || (cb.card.isDefined && visibleCards.contains(cb.card.get))
    }
  }
  def setEnabled(flag: Boolean) {
    cardButtons.foreach { btn ⇒
      btn.enabled = flag
      lastSelected.foreach(_.selected = false)
      lastSelected = None
    }
  }
}

class TopCardPanel(playerId: PlayerId, game: Game) {
  val houseLabels = game.desc.players(playerId).houses.zipWithIndex.map {
    case (houseDesc, idx) ⇒
      new HouseLabel(new DamagableInt(game.state.players(playerId).houses(idx).mana, game), houseDesc.house, game, flip = true)
  }
  val panel = Row(houseLabels)
  def refresh(silent: Boolean) { houseLabels.foreach(_.mana.refresh(silent)) }
}

