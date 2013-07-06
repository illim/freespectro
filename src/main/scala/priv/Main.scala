package priv

import javax.swing._
import java.awt.event._
import sp._
import org.lwjgl.opengl.{AWTGLCanvas, PixelFormat}
import priv.util.Utils._

object Main extends JFrame with App {
  val fgc = getGraphicsConfiguration
  val gbounds = fgc.getBounds()
  val mode = InitDisplay.findDisplayMode(gbounds.height, 32).get
  setSize(mode.getWidth, mode.getHeight)
  setExtendedState(java.awt.Frame.MAXIMIZED_VERT)
  setUndecorated(true)
  setLocation(gbounds.width/2 - mode.getWidth/2, gbounds.height/2 - mode.getHeight / 2)

  val panel = getContentPane
  panel.setLayout(new BoxLayout(panel, BoxLayout.PAGE_AXIS))
  val canvas = new AWTGLCanvas(new PixelFormat().withDepthBits(24).withSamples(4).withSRGB(true))
  val settingsPanel = new SettingsPanel
  settingsPanel.setVisible(false)
  panel.add(settingsPanel)
  panel.add(canvas)
  show()
  canvas.requestFocus()

  thread("render"){
    val r = new MainRender(canvas, mode, settingsPanel)
    if (!Shader.isSupported){
      r.world.ended = true
      println("Shader not supported")
    }
    addWindowListener(new WindowAdapter {
      override def windowClosing(e : WindowEvent) {
        r.world.ended = true
      }
    })
    doInDispatch {
      settingsPanel.tabs.addTab("houses", new GameSettings(r.resources))
      settingsPanel.tabs.addTab("multi", new MultiSettings(r.world, r.resources, { gameServer =>
        r.currentGame = r.createGame(gameServer)
      }))
      settingsPanel.tabs.addTab("debug", new GameDebug(r.currentGame))
    }
    r.mainLoop()
    dispose()
  }

  class SettingsPanel extends JPanel with ActionListener {
    setLayout(new BoxLayout(this, BoxLayout.PAGE_AXIS))
    alignX(addBtn("hide", this), java.awt.Component.RIGHT_ALIGNMENT)
    val tabs = new JTabbedPane
    add(tabs)

    def display() {
      doInDispatch {
        setVisible(!isVisible())
        if (isVisible()) {
          canvas.requestFocus() // dunno, black magic?
          canvas.setEnabled(false)
          canvas.setSize(1024, 400)
        } else {
          canvas.setEnabled(true)
          canvas.setSize(1024, 768)
        }
        panel.repaint()
        panel.revalidate()
      }
    }

    def actionPerformed(e : ActionEvent){
      e.getActionCommand() match {
        case "hide" => display()
      }
    }
  }

}
