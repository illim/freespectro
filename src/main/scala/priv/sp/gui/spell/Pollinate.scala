package priv.sp.gui.spell

import priv._
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL20._
import priv.sp._
import scala.util.Random._
import Coord2i._

class Pollen(texture : Texture, val startPos : Coord2i, dir : Coord2i) extends Particle {
  val lifeTime = 500
  val size = texture.size * nextFloat
  def render(t : Long) {
    val a = Coord2i(
      startPos.x + (dir.x * t/40).toInt,
      startPos.y - 50 - (dir.y * pow2(t / 50) / 2).toInt)
    tex.drawAt(a, texture.id, size)
  }
}
class PollenEmitter(sp : SpWorld, pos : Coord2i) extends Emitter with Entity {
  val rate = 10
  val maxTime = 800
  def getRandomPoint = Coord2i(pos.x -40 + nextInt(80), pos.y + nextInt(40))
  def build(time : Long) = {
    val startPos = getRandomPoint
    val dir = Coord2i(math.signum(startPos.x - pos.x) * nextInt(5), -1 * nextInt(5))
    new Pollen(sp.baseTextures.pollen, startPos, dir)
  }
}

class Pollinate(pos : Coord2i, sp : SpWorld) extends TimedEntity with Attachable {
  val duration = 1000L
  val emitter = new PollenEmitter(sp, pos)
  spawn(emitter)

  override def render() {
    glColor4f(1,1,1, 1 - (getDelta() / duration.toFloat))
    super.render()
  }
}
