package priv.sp.gui

import priv._
import org.lwjgl.opengl.GL11._
import priv.sp.PlayerState
import priv.sp.SpWorld

class LifeLabel(life : => Int) extends GuiElem {

  val size = Coord2i(30, 30)

  def render(world: World) {
    Fonts.draw(10, 22, life, 'white)
  }

}
