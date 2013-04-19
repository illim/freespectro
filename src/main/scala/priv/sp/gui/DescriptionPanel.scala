package priv.sp.gui

import priv._
import org.lwjgl.opengl.GL11._
import priv.sp._

class DescriptionPanel(game : Game) extends GuiElem {
  val size = Coord2i(200, 200)
  var cardOption = Option.empty[Card]

  def render() {
    cardOption.foreach{ card =>
      Fonts.big.draw(0, 0, card.name, 'white)
      card match {
        case c : Creature =>
          Fonts.font.draw(0, 25, "Life : " + c.life+"  Attack : " + c.attack.getOrElse("X"), 'white)
        case _ =>
      }
      Fonts.font.draw(0, 37, card.description, 'white)
    }
  }
}
