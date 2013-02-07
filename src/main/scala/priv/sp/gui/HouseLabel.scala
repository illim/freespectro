package priv.sp.gui

import priv._
import priv.sp.PlayerHouse
import priv.sp.SpWorld

class HouseLabel (house : PlayerHouse, sp : SpWorld) extends GuiElem {
  val texture = sp.textures.get("Images/Combat/NormalItem.tga")

  val size = Coord2i(texture.width, texture.height)

  def render(world: World) {
    drawTexture(texture)
    Fonts.draw(10, 22, house.house.name + " : " + house.mana)
  }

}