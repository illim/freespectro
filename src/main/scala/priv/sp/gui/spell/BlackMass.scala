package priv.sp.gui.spell

import priv._
import priv.sp.gui._
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL20._
import priv.sp._
import scala.util.Random._
import Coord2i._

class Mass(val startPos : Coord2i, dir : Coord2i, yLimit : Int) extends Particle {
  val lifeTime = 800
  def render(t : Long) {
    val a = startPos + dir * (t / 20f)
    var b = a + dir * 2
    if ((dir.y < 0 && b.y < yLimit) || (dir.y > 0 && b.y > yLimit)){
      b = Coord2i(a.x -1 + nextInt(3), a.y - 1 + nextInt(3))
    }
    glVertex2f(a.x, a.y)
    glVertex2f(b.x, b.y)
  }
}
class MassEmitter(pos : Coord2i, target : SlotPanel) extends Emitter with Entity {
  val rate = 20
  val maxTime = 800
  val offset = target.panel.coord + target.slotOffset
  val slotsSize = target.slotSize.xProj * 6
  def getRandomPoint = Coord2i(offset.x + nextInt(slotsSize.x), offset.y + nextInt(slotsSize.y))
  val down = offset.y > pos.y
  def getDir(p : Coord2i) = (pos - p) * 0.05f
  def build(time : Long) = {
    val p = getRandomPoint
    new Mass(p, getDir(p), pos.y)
  }
}

class BlackMass(pos : Coord2i, target : SlotPanel) extends TimedEntity with Attachable {
  val duration = 1000L
  val emitter = new MassEmitter(pos, target)
  spawn(emitter)

  override def render() {
    glDisable(GL_TEXTURE_2D)
    glColor4f(0.6f, 0.6f, 0.6f, 1)
    glBegin(GL_LINES)
    super.render()
    glEnd()
    glEnable(GL_TEXTURE_2D)
  }
}
