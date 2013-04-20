package priv.sp.gui

import priv._
import org.lwjgl.opengl.GL11._
import priv.sp._

class HouseLabel (val mana : DamagableInt, house : House, game : Game, flip : Boolean = false) extends GuiElem with Damagable {
  val size = Coord2i(102, 54)
  val direction = if (flip) -1 else 1

  def render() {
    glColor4f(1, 1, 1, 1)
    Fonts.font.draw(10, 22, house.name + " : " + mana.current, 'white)
    mana.getDamageAnimOpt.foreach{ anim =>
      Fonts.font.draw(20, 22 - direction * ( 10 + anim.delta(world.time)), anim.text, anim.color)
    }
  }

}
