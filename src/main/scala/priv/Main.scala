package priv

import scala.util.continuations._
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

  val multi = new Multi
  multi.show()

  val world = new World(g)
  val resources= new GameResources
  var currentGame = createGame()

  mainLoop()
  resources.release()
  currentGame.server.release()
  g.cleanUp()
  multi.dispose()

  private def init(gm : Game) : Game = {
    offsety = 0
    world.forEntity[GuiElem](_.updateCoord(Coord2i(0, 0)))
    world.entities.add(Repere)
    gm.surrenderButton.on{ case MouseClicked(c) =>
      currentGame = createGame()
    }
    gm
  }

  def createGame(gameServer : GameServer = new Local(resources) ) = {
    world.clear()
    val game = new Game(world, resources, gameServer )
    init(game)
    game
  }

  private def mainLoop() {
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
          case e : GuiEvent => world.forEntity[GuiContainer](_.fireEvent(e))
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
  }

  private def pollInput() = {
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
      val diff = math.abs(newy - g.height/2) - g.height/2
      val dy = if (diff > 0) - y + math.signum(y) * diff else -y
      offsety = offsety + dy
      glTranslatef(0, dy, 0)
      world.forEntity[GuiElem](_.updateCoord(Coord2i(0, offsety)))
    }
  }

  private def clearScreen() {
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
    glMatrixMode(GL_MODELVIEW)
  }

  import javax.swing._
  import java.awt.event._
  class Multi extends JFrame with ActionListener {
    val p = new JPanel
    def addBtn(name : String){
      val b = new JButton(name)
      b.setActionCommand(name)
      b.addActionListener(this)
      p.add(b)
    }
    setSize(200, 200)
    val ipTxts = (0 to 3).map{ _ =>
      val t = new JTextField(3)
      p.add(t)
      t
    }
    val connectBtn = addBtn("connect")
    val serveBtn = addBtn("serve")
    add(p)

    def actionPerformed(e : ActionEvent){
      e.getActionCommand() match {
        case "serve" =>
          currentGame.server.release()
          val gameServer = new Master(resources)
          world.doInRenderThread{
            currentGame = createGame(gameServer)
          }
        case "connect" =>
          currentGame.server.release()
          val address = java.net.InetAddress.getByAddress(ipTxts.map{_.getText().toByte}.toArray)
          reset {
            val gameServer = shift { k: (GameServer => Unit) =>
              new SlaveBoot(k, address, resources)
            }
            world.doInRenderThread{
              currentGame = createGame(gameServer)
            }
          }
      }
    }
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
