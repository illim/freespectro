package priv.entity

import org.lwjgl.opengl.GL11._
import priv.util.Once

object Coord {
  val zero = Coord(0, 0)
}

case class Coord(x: Float, y: Float)

object GuiHandler {
  type Type = PartialFunction[MouseEvent, Unit]
}

sealed trait MouseEvent {
  def coord: Coord
}
case class MouseMoved(coord: Coord) extends MouseEvent
case class MouseClicked(coord: Coord) extends MouseEvent
case class MouseLeaved(coord: Coord) extends MouseEvent

trait GuiElem extends Entity {
  def size: Coord
  def clean() {}
  var handlers = List.empty[GuiHandler.Type]
  def on(handler: GuiHandler.Type) {
    handlers ::= handler
  }
  def handle(mouseEvent: MouseEvent) = {
    handlers.foreach { handler =>
      if (handler.isDefinedAt(mouseEvent)) {
        handler(mouseEvent)
      }
    }
  }
}

trait GuiContainer extends GuiElem {

  var elems = List.empty[(Coord, GuiElem)]
  protected def add(coord: Coord, elem: GuiElem) {
    elems ::= (coord, elem)
  }
  override def clean() {
    elems.foreach(_._2.clean())
  }
  def findElem(c: Coord): Option[GuiElem] = {
    (elems.collectFirst {
      case (pos, elem) if (c.x > pos.x
        && c.x < pos.x + elem.size.x
        && c.y > pos.y
        && c.y < pos.y + elem.size.y) =>
        elem match {
          case cont: GuiContainer => cont.findElem(Coord(c.x - pos.x, c.y - pos.y)) // sucks
          case e => Some(e)
        }
    }).flatten
  }

  var lastElem = Option.empty[GuiElem]
  def fireEvent(mouseEvent: MouseEvent) = {
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

case class Translate(at: Coord, elt: GuiElem) extends GuiContainer {
  val size = Coord(elt.size.x + at.x, elt.size.y + at.y)
  add(at, elt)

  def render(world: World) {
    glPushMatrix()
    glTranslatef(at.x, at.y, 0)
    elt.render(world)
    glPopMatrix()
  }
}

abstract class Flow(elts: Traversable[GuiElem], dirx: Int = 0, diry: Int = 0) extends GuiContainer {
  val last = (Coord(0, 0) /: elts) { (acc, elt) =>
    val c = acc.copy(x = acc.x + dirx * elt.size.x, y = acc.y + diry * elt.size.y)
    add(acc, elt)
    c
  }
  val size = if (dirx == 0) {
    last.copy(x = elts.headOption.map(_.size.x) getOrElse 0)
  } else {
    last.copy(y = elts.headOption.map(_.size.y) getOrElse 0)
  }

  def render(world: World) {
    glPushMatrix()
    elts.foreach { elt =>
      elt.render(world)
      glTranslatef(dirx * elt.size.x, diry * elt.size.y, 0)
    }
    glPopMatrix()
  }
}

case class Column(elts: Traversable[GuiElem]) extends Flow(elts, diry = 1)

case class Row(elts: Traversable[GuiElem]) extends Flow(elts, dirx = 1)
