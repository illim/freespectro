package priv.sp.gui.spell

import priv._
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL20._
import priv.sp._
import scala.util.Random._
import Coord2i._

class NatureLight(texture : Texture, val startPos : Coord2i, dir : Coord2i) extends Particle {
  val lifeTime = 500
  val size = texture.size * nextFloat
  def render(t : Long) {
    val a = startPos + dir * (t / 30f)
    tex.drawAt(a, texture.id, size)
  }
}
class NatureLightEmitter(sp : SpWorld, pos : Coord2i) extends Emitter with Entity {
  val rate = 20
  val maxTime = 800
  def getRandomPoint = Coord2i(pos.x -40 + nextInt(80), pos.y + nextInt(40))
  val dir = Coord2i(0, -5)
  def build(time : Long) = new NatureLight(sp.baseTextures.natureLight, getRandomPoint, dir)
}

class NatureRitual(pos : Coord2i, sp : SpWorld) extends TimedEntity with Attachable {
  val duration = 1000L
  val emitter = new NatureLightEmitter(sp, pos)
  spawn(emitter)

  override def render() {
    glColor4f(1,1,1, 1 - (getDelta() / duration.toFloat))
    super.render()
  }
}
