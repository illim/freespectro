package priv


import collection.JavaConversions._
import org.lwjgl.opengl._
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL12._
import org.lwjgl.opengl.GL13._
import org.lwjgl.opengl.GL15._
import org.lwjgl.opengl.GL20._
import sp._
import org.lwjgl.input.Mouse
import priv.util.Repere

class MainRender(canvas : java.awt.Canvas, mode : DisplayMode, settingsPanel : Main.SettingsPanel) {
  private val g = InitDisplay(canvas, mode)
  private var offsety = 0
  val world = new World(g)
  val resources= new GameResources
  var currentGame = createGame()

  private def init(gm : Game) : Game = {
    glTranslatef(0, - offsety, 0)
    offsety = 0
    world.forEntity[GuiElem](_.updateCoord(Coord2i(0, 0)))
    world.spawn(Repere)
    gm.surrenderButton.on{ case MouseClicked(c) =>
      resources.updateExecutor.releaseLock()
      currentGame = createGame()
    }
    gm.settingsButton.on{ case MouseClicked(c) =>
      settingsPanel.display()
    }
    gm
  }

  def createGame(gameServer : GameServer = new Local(resources) ) = {
    world.clear()
    val game = new Game(world, resources, gameServer )
    init(game)
    game
  }

  def mainLoop() {
    while (!Display.isCloseRequested() && ! world.ended) {
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
    resources.release()
    g.cleanUp()
  }

  private def pollInput() = {
    var result = Option.empty[MouseEvent]
    while (Mouse.next()) {
      result = if (Mouse.getEventButton() != -1 && Mouse.isButtonDown(0)) {
        Some(MouseClicked(getMouseCoord))
      } else {
        val wheel = Mouse.getDWheel()
        if  (Mouse.isButtonDown(0)){
          Some(MouseDrag(Mouse.getDY()))
        } else if (wheel != 0){
          Some(MouseWheel(wheel))
        } else {
          Some(MouseMoved(getMouseCoord))
        }
      }
    }
    result
  }

  private def getMouseCoord() = {
    val p = canvas.getLocation
    Coord2i(Mouse.getX() - p.x, g.height - offsety - Mouse.getY() - p.y)
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
