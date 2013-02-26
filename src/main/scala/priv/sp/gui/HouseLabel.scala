package priv.sp.gui

import priv._
import org.lwjgl.opengl.GL11._
import priv.sp._

class HouseLabel (houseState : => HouseState, house : House, sp : SpWorld, flip : Boolean = false) extends GuiElem {
  val texture = sp.textures.get("Images/Combat/NormalItem.tga")

  val size = Coord2i(texture.width, texture.height)

  def render(world: World) {
    glColor4f(1, 1, 1, 1)
    tex.draw(texture.id, texture.coord, flip)
    Fonts.draw(10, 22, house.name + " : " + houseState.mana)
  }

}
