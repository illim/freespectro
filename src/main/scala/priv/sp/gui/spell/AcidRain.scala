package priv.sp.gui.spell

import priv._
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL20._
import priv.sp._
import scala.util.Random._
import Coord2i._

// suboptimal

trait Particle {
  def startPos: Coord2i
  def lifeTime: Int
  var creationTime = 0L
  def render(t: Long)
}

trait Emitter { _: Entity ⇒
  def rate: Int
  def maxTime: Int
  def build(time: Long): Particle
  var particles = Vector.empty[Particle]
  var lastEmit = 0L

  def render() {
    val delta = getDelta()
    if (delta - lastEmit > rate && delta - creationTime < maxTime) {
      val p = build(delta)
      p.creationTime = delta
      particles = particles :+ p
      lastEmit = delta
    }
    particles foreach { p ⇒
      if (delta - p.creationTime > p.lifeTime) {
        particles = particles.filterNot(_ == p)
      } else {
        p.render(delta - p.creationTime)
      }
    }
  }
}

class RainDrop(val startPos: Coord2i, dir: Coord2i) extends Particle {
  val lifeTime = 900
  def render(t: Long) {
    val a = startPos + dir * (t / 30f)
    val b = a + dir
    glVertex2f(a.x, a.y)
    glVertex2f(b.x, b.y)
  }
}
class RainEmitter extends Emitter with Entity {
  val rate = 10
  val maxTime = 1300
  def getRandomPoint = Coord2i(100 + nextInt(600), nextInt(50))
  val dir = Coord2i(-2, 5)
  def build(time: Long) = new RainDrop(getRandomPoint, dir)
}

class AcidRain extends TimedEntity with Attachable {
  val duration = 1500L
  val emitter = new RainEmitter
  spawn(emitter)

  override def render() {
    glDisable(GL_TEXTURE_2D)
    glColor4f(0.8f, 1, 0.6f, 1 - (getDelta() / duration.toFloat))
    glBegin(GL_LINES)
    super.render()
    glEnd()
    glEnable(GL_TEXTURE_2D)
  }
}
