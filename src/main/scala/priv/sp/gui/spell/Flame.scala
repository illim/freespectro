package priv.sp.gui.spell

import priv._
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL20._
import priv.sp._
import scala.util.Random
import Coord2i._

class Flame(sp: SpWorld, slotOffset: Coord2i, slotSize: Coord2i) extends TimedEntity {
  val duration = 1500L
  val fireTex = sp.baseTextures.fire
  val offset = Coord2i(slotOffset.x + slotSize.x / 2, slotSize.y / 2)
  val partTimeLine = baseSlotRange.toList flatMap { numSlot ⇒
    val nbPart = 4 + Random.nextInt(2)
    val slotOffset = offset.xProj + (slotSize.x * numSlot)
    (List.empty[(Int, Coord2i)] /: (0 to nbPart)) { (acc, n) ⇒
      val xf = 1 + n * (40 / nbPart)
      (Random.nextInt(5), slotOffset + Coord2i(Random.nextInt(xf) - xf / 2, Random.nextInt(10) - 5)) :: acc
    }
  }
  var currentPart = 0
  var shownParts = List.empty[(Coord2i, Long)]
  var nbPart = partTimeLine.size

  def render() {
    glBlendFunc(GL_SRC_ALPHA, GL_ONE)
    val delta = getDelta()
    if (currentPart < nbPart && delta > partTimeLine(currentPart)._1) {
      shownParts = (partTimeLine(currentPart)._2, delta) :: shownParts
      currentPart += 1
    }
    shownParts foreach {
      case (p, d) ⇒
        val cursor = delta - d
        val fact = (cursor / 20f).intValue
        val k = math.cos(cursor / 500f).floatValue
        glColor4f(k, k, k, k)
        val size = fireTex.size.yProj + fact
        tex.drawAt(recenter(p.yProj - fact, size), fireTex.id, size)
    }
  }
}
