package priv.sp.gui

import priv._
import org.lwjgl.opengl.GL11._
import priv.sp._

class LifeLabel(name : String, val life : DamagableInt, game : Game) extends GuiElem with Damagable {
  val nameWidth = Fonts.font.getWidth(name)
  val size = Coord2i(30, 50)
  var phaseOption = Option.empty[(Int, String)]
  def setPhase(s : String){
    if (s != null) phaseOption = Some((30 - Fonts.font.getWidth(s), s))
  }

  def render() {
    Fonts.font.draw(-nameWidth, 22, name, 'white)
    Fonts.font.draw(10, 22, life.current, 'white)
    life.getDamageAnimOpt.foreach{ anim =>
      Fonts.font.draw(10, 10 - anim.delta(world.time), anim.text, anim.color)
    }
    phaseOption.foreach{ case (pos, phase) =>
      Fonts.font.draw(pos, 35, phase, 'gray)
    }
  }

}
