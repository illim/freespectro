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
import org.lwjgl.util.glu.GLU._
import javax.swing._

object InitDisplay {

  def findDisplayMode(width: Int, height: Int, bpp: Int) = {
    val modes = Display.getAvailableDisplayModes()
    modes.find { mode =>
      (mode.getWidth() == width && mode.getHeight() == height && mode.getBitsPerPixel() >= bpp)
    }
  }

  def apply(canvas : java.awt.Canvas, mode : DisplayMode) = {
    Display.setDisplayMode(mode)
    Display.setParent(canvas)
    Display.create()
    Display.setVSyncEnabled(true)
    initGLState(mode)
    Keyboard.create()

    DisplayConf(Display.getWidth, Display.getHeight)
  }

  private def initGLState(mode: DisplayMode) {
    def initDepth() {
      glClearDepth(1.0f)
      glEnable(GL_DEPTH_TEST)
      glDepthFunc(GL_LEQUAL)
    }

    initDepth()
    glEnable(GL_ALPHA_TEST)
    glEnable(GL_BLEND)
    glEnable(GL_TEXTURE_2D)
    glClearColor(0f, 0f, 0f, 0.5f) //set clear color to black
    glOrtho(0, mode.getWidth(), mode.getHeight, 0, -1, 1)
    glViewport(0, 0, mode.getWidth(), mode.getHeight)
    org.lwjgl.opengl.Util.checkGLError()
  }
}

case class DisplayConf(val width: Int, val height: Int) {
  val resolution = Coord2i(width, height)

  val offscreenTex = new OffscreenTexture(width, height)

  def saveScreen() {
    glLoadIdentity()
    glBindTexture(GL_TEXTURE_2D, offscreenTex.id)
    glCopyTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, 0, 0, width, height)
  }

  def cleanUp() {
    Display.destroy()
  }
}
