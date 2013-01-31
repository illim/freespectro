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
import entity._
import sp._
import org.lwjgl.input.Mouse
import priv.sp.gui.Board

object Main extends Application {
  val g = GInit()
  mainLoop()
  g.cleanUp()

  val screenTexId = 0

  def mainLoop() {
    val spWorld = new SpWorld    
    val gsm = new GameStateMachine()
    val board = new Board(gsm, spWorld)
    val res = Seq(Repere)
    val texRes = Seq(board)
    val world = new World()
    
    res.foreach(world.entities.add)
    texRes.foreach(world.texEntities.add)

    while (!Keyboard.isKeyDown(Keyboard.KEY_ESCAPE) && !Display.isCloseRequested()) {
      if (Display.isVisible()) {
        clearScreen()
        world.tick()
        glPushMatrix()
        world.render()
        glPopMatrix()
        //render()

        //g.saveScreen()
        pollInput().foreach(board.panel.fireEvent _)
        Display.update()
        Display.sync(60)

        // check keyboard input
        //  	    processKeyboard()
        // do "game" logic, and render it
        //  	    logic()
        //  	    render()
      } else {
        // no need to render/paint if nothing has changed (ie. window
        // dragged over)
        if (Display.isDirty()) {
          //  	      render(resources)
        }
        // don't waste cpu time, sleep more
        try {
          Thread.sleep(100)
        } catch {
          case _ : Throwable =>
        }
      }
    }
    res.foreach(_.clean())
    spWorld.textures.clean()
  }

  def pollInput() = {
    var result = Option.empty[MouseEvent]
    while (Mouse.next()) {
      result = if (Mouse.getEventButton() != -1 && Mouse.isButtonDown(0)) {
        Some(MouseClicked(Coord(Mouse.getX(), g.height - Mouse.getY())))
      } else Some(MouseMoved(Coord(Mouse.getX(), g.height - Mouse.getY())))
    }
    result
  }

  private def clearScreen() {
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
    glMatrixMode(GL_MODELVIEW)
  }
}
