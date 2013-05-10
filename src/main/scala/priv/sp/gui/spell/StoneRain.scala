package priv.sp.gui.spell

import priv._
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL20._
import priv.sp._
import scala.util.Random._
import Coord2i._

class Stone(texture : Texture, val startPos : Coord2i, dir : Coord2i) extends Particle {
  val tfact = 10 + nextInt(10)
  val lifeTime = 4000 / tfact
  def render(t : Long) {
    val a = startPos + dir * (t / tfact.toFloat)
    tex.drawAt(a, texture.id, texture.size)
  }
}
class StoneEmitter(sp : SpWorld) extends Emitter with Entity {
  val rate = 20
  val maxTime = 1300
  def getRandomPoint = Coord2i(50 + nextInt(600), nextInt(50))
  def getRandomTexture = {
    val stones = sp.baseTextures.stones
    stones(nextInt(stones.size))
  }
  val dir = Coord2i(2, 5)
  def build(time : Long) = new Stone(getRandomTexture, getRandomPoint, dir)
}

class StoneRain(sp : SpWorld) extends TimedEntity with Attachable {
  val duration = 1500L
  val emitter = new StoneEmitter(sp)
  spawn(emitter)

  override def render() {
    glColor4f(1, 1, 1, 1)
    super.render()
  }
}
