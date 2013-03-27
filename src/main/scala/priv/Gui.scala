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
    handlers.foreach { handler =>
      if (handler.isDefinedAt(mouseEvent)) {
        handler(mouseEvent)
      }
    }
  }
  def updateCoord(c : Coord2i){ coord = c }
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
        && c.y < pos.y + elem.size.y) =>
        elem match {
          case cont: GuiContainer => cont.findElem(Coord2i(c.x - pos.x, c.y - pos.y)) // sucks
          case e => Some(e)
        }
    }).flatten
  }

  var lastElem = Option.empty[GuiElem]
  def fireEvent(mouseEvent: GuiEvent) = {
    findElem(mouseEvent.coord).foreach { elem =>
      if (Some(elem) != lastElem) {
        lastElem.foreach { e =>
          if (e != elem) {
            e.handle(MouseLeaved(mouseEvent.coord))
          }
        }
      }
      elem.handle(mouseEvent) // todo filter redondant move?
      lastElem = Some(elem)
    }
  }
}

case class Translate(at: Coord2i, elt: GuiElem) extends GuiContainer {
  val size = Coord2i(elt.size.x + at.x, elt.size.y + at.y)
  add(at, elt)

  def render(world: World) {
    glPushMatrix()
    glTranslatef(at.x, at.y, 0)
    elt.render(world)
    glPopMatrix()
  }
  override def updateCoord(c : Coord2i){
    super.updateCoord(c)
    elt.updateCoord(c + at)
  }
}

abstract class Flow(dirx: Int = 0, diry: Int = 0) extends GuiContainer with Attachable {
  def elts: Traversable[GuiElem]

  val last = (Coord2i(0, 0) /: elts) { (acc, elt) =>
    val c = acc.copy(x = acc.x + dirx * elt.size.x, y = acc.y + diry * elt.size.y)
    add(acc, elt)
    c
  }
  val size = if (dirx == 0) {
    last.copy(x = if (elts.isEmpty) 0 else elts.maxBy(_.size.x).size.x)
  } else {
    last.copy(y = if (elts.isEmpty) 0 else elts.maxBy(_.size.y).size.y)
  }

  def render(world: World) {
    glPushMatrix()
    elts.foreach { elt =>
      elt.render(world)
      glTranslatef(dirx * elt.size.x, diry * elt.size.y, 0)
    }
    glPopMatrix()
    renderAttacheds(world)
  }

  override def updateCoord(c : Coord2i){
    super.updateCoord(c)
// todo update attached stuff?
    (c /: elts) { (acc, elt) =>
      elt.updateCoord(acc)
      acc.copy(x = acc.x + dirx * elt.size.x, y = acc.y + diry * elt.size.y)
    }
  }
}

case class Column(elts: Traversable[GuiElem]) extends Flow(diry = 1)

case class Row(elts: Traversable[GuiElem]) extends Flow(dirx = 1)


class GuiButton(name : String) extends GuiElem {
  val (w, h) = (Fonts.getWidth(name), Fonts.getHeight(name))
  val size = Coord2i(w, h)

  def render(world: World) {
    Fonts.draw(0, 0, name, 'white)
  }
}
