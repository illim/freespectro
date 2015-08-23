package priv

import org.lwjgl.opengl.GL11._
import priv._
import collection._

/**
 * Supposing that the ui is completely static
 */
object GuiHandler {
  type Type = PartialFunction[GuiEvent, Unit]
}

trait GuiElem extends Entity {
  var coord = Coord2i(0, 0)
  def size: Coord2i
  var handlers = List.empty[GuiHandler.Type]
  var enabled = true
  def on(handler: GuiHandler.Type) {
    handlers ::= handler
  }
  def handle(mouseEvent: GuiEvent) = {
    handlers.foreach { handler ⇒
      if (handler.isDefinedAt(mouseEvent)) {
        handler(mouseEvent)
      }
    }
  }
  def updateCoord(c: Coord2i) { coord = c }
}

trait GuiContainer extends GuiElem {

  var elems = List.empty[(Coord2i, GuiElem)]
  protected def add(coord: Coord2i, elem: GuiElem) {
    elems ::= (coord, elem)
  }
  def findElem(c: Coord2i): Option[GuiElem] = {
    (elems.collectFirst {
      case (pos, elem) if (c.x > pos.x
        && c.x < pos.x + elem.size.x
        && c.y > pos.y
        && c.y < pos.y + elem.size.y) ⇒
        elem match {
          case cont: GuiContainer ⇒ cont.findElem(Coord2i(c.x - pos.x, c.y - pos.y)) // sucks
          case e                  ⇒ Some(e)
        }
    }).flatten
  }

  var lastElem = Option.empty[GuiElem]
  def fireEvent(mouseEvent: GuiEvent) = {
    val elemOption = findElem(mouseEvent.coord)
    if (lastElem.isDefined && elemOption != lastElem) {
      lastElem.get.handle(MouseLeaved(mouseEvent.coord))
    }
    elemOption.foreach { elem ⇒
      elem.handle(mouseEvent) // todo filter redondant move?
      lastElem = Some(elem)
    }
  }
  override def setWorld(w: World) {
    super.setWorld(w)
    elems.foreach(_._2.setWorld(w)) // supposing no elem is added to the container after spawn
  }
}

case class Translate(at: Coord2i, elt: GuiElem) extends GuiContainer {
  val size = Coord2i(elt.size.x + at.x, elt.size.y + at.y)
  add(at, elt)

  def render() {
    glPushMatrix()
    glTranslatef(at.x, at.y, 0)
    elt.render()
    glPopMatrix()
  }
  override def updateCoord(c: Coord2i) {
    super.updateCoord(c)
    elt.updateCoord(c + at)
  }
}

abstract class Flow(dirx: Int = 0, diry: Int = 0) extends Attachable with GuiContainer {
  def elts: Traversable[GuiElem]

  val last = (Coord2i(0, 0) /: elts) { (acc, elt) ⇒
    val c = acc.copy(x = acc.x + dirx * elt.size.x, y = acc.y + diry * elt.size.y)
    add(acc, elt)
    c
  }
  val size = if (dirx == 0) {
    last.copy(x = if (elts.isEmpty) 0 else elts.maxBy(_.size.x).size.x)
  } else {
    last.copy(y = if (elts.isEmpty) 0 else elts.maxBy(_.size.y).size.y)
  }

  spawn(new Entity {
    def render() {
      glPushMatrix()
      elts.foreach { elt ⇒
        elt.render()
        glTranslatef(dirx * elt.size.x, diry * elt.size.y, 0)
      }
      glPopMatrix()
    }
  })

  override def updateCoord(c: Coord2i) {
    super.updateCoord(c)
    // todo update attached stuff?
    (c /: elts) { (acc, elt) ⇒
      elt.updateCoord(acc)
      acc.copy(x = acc.x + dirx * elt.size.x, y = acc.y + diry * elt.size.y)
    }
  }
}

case class Column(elts: Traversable[GuiElem]) extends Flow(diry = 1)

case class Row(elts: Traversable[GuiElem]) extends Flow(dirx = 1)

class GuiButton(name: String, font: PimpFont = Fonts.font) extends GuiElem {
  val (w, h) = (font.getWidth(name), font.getHeight(name))
  val size = Coord2i(w, h)

  def render() {
    font.draw(0, 0, name, 'white)
  }
}
