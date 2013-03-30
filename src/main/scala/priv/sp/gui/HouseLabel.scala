package priv.sp.gui

import priv._
import org.lwjgl.opengl.GL11._
import priv.sp._

class HouseLabel (val mana : DamagableInt, house : House, game : Game, flip : Boolean = false) extends GuiElem with Damagable {
  val texture = game.sp.textures.get("Images/Combat/NormalItem.tga")
  val size = texture.size
  val direction = if (flip) -1 else 1

  def render(world: World) {
    glColor4f(1, 1, 1, 1)
    tex.draw(texture.id, texture.size, flip)
    Fonts.font.draw(10, 22, house.name + " : " + mana.current)
    mana.getDamageAnimOpt.foreach{ anim =>
      Fonts.font.draw(20, 22 - direction * ( 10 + anim.delta(world.time)), anim.text, anim.color)
    }
  }

}
