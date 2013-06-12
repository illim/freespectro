package priv.sp.gui.spell

import priv._
import priv.sp.gui._
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL20._
import priv.sp._
import scala.util.Random._
import Coord2i._

class EarthQuake(game : Game) extends TimedEntity with ShaderEntity {
  type S = RippleShader
  val duration = 1000L
  val shader = game.sp.baseShaders.ripple

  override def render() {
    val deltax = getDelta() / 10f
    val animationCursor = deltax.intValue % duration
    glUniform1i(shader.cursor, animationCursor.intValue)
  }
}
