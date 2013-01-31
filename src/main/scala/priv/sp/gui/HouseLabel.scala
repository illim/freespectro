package priv.sp.gui
import priv._
import priv.entity.Coord
import priv.entity.GuiElem
import priv.entity.World
import priv.sp.PlayerHouse
import priv.sp.SpWorld

class HouseLabel (house : PlayerHouse, sp : SpWorld) extends GuiElem {
  val texture = sp.textures.get("Images/Combat/NormalItem.tga")

  val size = Coord(texture.width, texture.height)

  def render(world: World) {
    drawTexture(texture)
    Fonts.draw(30, 22, house.house.name + " : " + house.mana)
  }

}