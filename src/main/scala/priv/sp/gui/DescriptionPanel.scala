package priv.sp.gui

import priv._
import org.lwjgl.opengl.GL11._
import priv.sp._

object DescriptionPanel {
  def show(offset: Int, color: Symbol)(described: Described) = {
    var cur = offset
    Fonts.big.draw(0, cur, described.name, color)
    cur += 25
    described match {
      case c: Creature ⇒
        Fonts.font.draw(0, cur, "Life : " + c.life + "  Attack : " + c.attack.base.getOrElse("X"), color)
        cur += 12
      case _ ⇒
    }
    if (described.description.size < 60) {
      Fonts.font.draw(0, cur, described.description, color)
    } else {
      described.description.split('\n').foreach { line ⇒
        Fonts.font.draw(0, cur, line, color)
        cur += 12
      }
    }
    cur
  }
}
class DescriptionPanel(game: Game) extends GuiElem {
  val size = Coord2i(200, 200)
  var describedOption = Option.empty[Described]
  val show = DescriptionPanel.show(0, 'white) _

  def render() {
    describedOption foreach show
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
    cardOption foreach show
  }
}
