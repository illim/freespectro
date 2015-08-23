package priv.sp.gui.spell

import priv._
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL20._
import priv.sp._
import scala.util.Random
import Coord2i._

class Lightning(sp: SpWorld, points: Coord2i*) extends TimedEntity {
  val lines = split(points.zip(points.tail).map {
    case (p1, p2) ⇒
      new Line(p1, p2)
  }, 10)
  val duration = 1500L

  def render() {
    glDisable(GL_TEXTURE_2D)
    glColor4f(1, 1, 1, 1 - (getDelta() / duration.toFloat))
    glBegin(GL_LINES)
    lines.foreach { line ⇒
      glVertex2f(line.a.x, line.a.y)
      glVertex2f(line.b.x, line.b.y)
    }
    glEnd()
    glEnable(GL_TEXTURE_2D)
  }

  case class Line(a: Coord2i, b: Coord2i) {
    def smallOff = 0.4f + 0.2f * Random.nextFloat
    def split() = {
      val dir = (b - a)
      val (xfact, yfact) = (smallOff, smallOff)
      val diro = dir.copy(x = (dir.x * xfact).toInt, y = (dir.y * yfact).toInt)
      val c = a + diro
      List(Line(a, c), Line(c, b))
    }
  }

  def split(ss: Seq[Line], n: Int): Seq[Line] = {
    if (n == 0) {
      ss
    } else {
      split(ss.flatMap(_.split()), n - 1)
    }
  }
}
