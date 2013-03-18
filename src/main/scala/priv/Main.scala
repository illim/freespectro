package priv

import collection.JavaConversions._
import java.nio.ByteBuffer
import java.nio.ByteOrder
import java.nio.IntBuffer
import org.lwjgl.LWJGLException
import org.lwjgl.input.Keyboard
import org.lwjgl.opengl._
import org.lwjgl.util.vector.Vector2f
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL12._
import org.lwjgl.opengl.GL13._
import org.lwjgl.opengl.GL15._
import org.lwjgl.opengl.GL20._
import org.lwjgl.util.glu.GLU._
import org.lwjgl.opengl.Util.checkGLError
import sp._
import org.lwjgl.input.Mouse
import priv.util.Repere

object Main extends App {
  var offsety = 0
  val g = GInit()

  mainLoop()
  g.cleanUp()

  def mainLoop() {
    val world = new World(g)
    val game = new Game(world)
    world.entities.add(Repere)
    world.forGuiElem(_.updateCoord(Coord2i(0, 0))) //useless now

    while (!Keyboard.isKeyDown(Keyboard.KEY_ESCAPE) && !Display.isCloseRequested() && ! world.ended) {
      if (Display.isVisible()) {
        clearScreen()
        world.tick()
        glPushMatrix()
        world.render()
        glPopMatrix()
        pollInput().foreach{
          case MouseDrag(y) => scroll(world, y)
          case MouseWheel(w) => scroll(world, -w)
          case e : GuiEvent => game.board.panel.fireEvent(e)
        }
        Display.update()
        Display.sync(60)
      } else {
        try {
          Thread.sleep(100)
        } catch {
          case _: Throwable =>
        }
      }
    }
    println("releasing resources")
    game.sp.clean()
    game.aiExecutor.shutdown()
  }

  def pollInput() = {
    var result = Option.empty[MouseEvent]
    while (Mouse.next()) {
      result = if (Mouse.getEventButton() != -1 && Mouse.isButtonDown(0)) {
        Some(MouseClicked(Coord2i(Mouse.getX(), g.height - offsety - Mouse.getY())))
      } else {
        val wheel = Mouse.getDWheel()
        if  (Mouse.isButtonDown(0)){
          Some(MouseDrag(Mouse.getDY()))
        } else if (wheel != 0){
          Some(MouseWheel(wheel))
        } else {
          Some(MouseMoved(Coord2i(Mouse.getX(), g.height - offsety - Mouse.getY())))
        }
      }
    }
    result
  }

  private def scroll(world : World, y : Int){
    if (math.abs(y) > 2){
      val newy = offsety - y
      val dy = if (math.abs(newy - g.height/2) > g.height/2) 0 else -y //todo diff instead of 0
      offsety = offsety + dy
      glTranslatef(0, dy, 0)
      world.forGuiElem(_.updateCoord(Coord2i(0, offsety))) // useless now
    }
  }

  private def clearScreen() {
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
    glMatrixMode(GL_MODELVIEW)
  }
}


sealed trait MouseEvent

sealed trait GuiEvent extends MouseEvent{
  def coord: Coord2i
}
case class MouseMoved(coord: Coord2i) extends GuiEvent
case class MouseClicked(coord: Coord2i) extends GuiEvent
case class MouseLeaved(coord: Coord2i) extends GuiEvent
case class MouseDrag(y : Int) extends MouseEvent
case class MouseWheel(w : Int) extends MouseEvent
