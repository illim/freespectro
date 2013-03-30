package priv.sp.gui

import priv._
import org.lwjgl.opengl.GL11._
import priv.sp._

class LifeLabel(name : String, val life : DamagableInt, game : Game) extends GuiElem with Damagable {
  val nameWidth = Fonts.font.getWidth(name)
  val size = Coord2i(30, 30)

  def render(world: World) {
    Fonts.font.draw(-nameWidth, 22, name, 'white)
    Fonts.font.draw(10, 22, life.current, 'white)
    life.getDamageAnimOpt.foreach{ anim =>
      Fonts.font.draw(10, 10 - anim.delta(world.time), anim.text, anim.color)
    }
  }

}
