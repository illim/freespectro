package priv.sp.gui

import priv._
import org.lwjgl.opengl.GL11._
import priv.sp._

object DescriptionPanel {
  def show(offset: Int, color: Symbol)(card: Card) = {
    var cur = offset
    Fonts.big.draw(0, cur, card.name, color)
    cur += 25
    card match {
      case c: Creature ⇒
        Fonts.font.draw(0, cur, "Life : " + c.life + "  Attack : " + c.attack.base.getOrElse("X"), color)
        cur += 12
      case _ ⇒
    }
    if (card.description.size < 60) {
      Fonts.font.draw(0, cur, card.description, color)
    } else {
      card.description.split('\n').foreach { line ⇒
        Fonts.font.draw(0, cur, line, color)
        cur += 12
      }
    }
    cur
  }
}
class DescriptionPanel(game: Game) extends GuiElem {
  val size = Coord2i(200, 200)
  var cardOption = Option.empty[Card]
  val show = DescriptionPanel.show(0, 'white) _

  def render() {
    cardOption.foreach(show)
  }
}

class InfoPanel(game: Game) extends GuiElem {
  val size = Coord2i(200, 200)
  var cardOption = Option.empty[Card]
  val show = DescriptionPanel.show(12, 'gray) _

  def add(c: Card) {
    cardOption = Some(c)
  }

  def render() {
    Fonts.font.draw(0, 0, "last play : ", 'gray)
    cardOption.foreach(show)
  }
}
