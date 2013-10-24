package priv

import collection.JavaConversions._
import java.nio._
import org.lwjgl.LWJGLException
import org.lwjgl.input.Keyboard
import org.lwjgl.opengl._
import org.lwjgl.util.vector.Vector2f
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL12._
import org.lwjgl.util.glu.GLU._
import javax.swing._
import util.GBufferUtils._

object InitDisplay {

  def findDisplayMode(height: Int, bpp: Int) = {
    val width = 4 * height / 3
    val modes = Display.getAvailableDisplayModes()
    modes.toList.map { mode =>
      (mode, (mode.getWidth() == width,  mode.getHeight() == height, bpp - mode.getBitsPerPixel()))
    }.sortBy(_._2).lastOption.map(_._1)
  }

  def apply(canvas : java.awt.Canvas, mode : DisplayMode) = {
    Display.setDisplayMode(mode)
    Display.setParent(canvas)
    Display.create()
    Display.setVSyncEnabled(true)
    initGLState(mode)
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

    initLights()

    glClearColor(0f, 0f, 0f, 0.5f) //set clear color to black
    glOrtho(0, 1024, 768, 0, -1, 1)
    glViewport(0, 0, mode.getWidth(), mode.getHeight)
    org.lwjgl.opengl.Util.checkGLError()
  }

  def initLights() {
		val matSpecular = createFloatBuffer(4)
		matSpecular.put(1.0f).put(1.0f).put(1.0f).put(1.0f).flip()
		val lightPosition = createFloatBuffer(4)
		lightPosition.put(0.0f).put(0.0f).put(-1.0f).put(0.0f).flip()
		val whiteLight = createFloatBuffer(4)
		whiteLight.put(1.0f).put(1.0f).put(1.0f).put(1.0f).flip()
		val lModelAmbient = createFloatBuffer(4)
		lModelAmbient.put(0.5f).put(0.5f).put(0.5f).put(1.0f).flip()

    glMaterial(GL_FRONT, GL_SPECULAR, matSpecular)
		glMaterialf(GL_FRONT, GL_SHININESS, 50.0f)

		glLight(GL_LIGHT0, GL_POSITION, lightPosition)
		glLight(GL_LIGHT0, GL_SPECULAR, whiteLight)
		glLight(GL_LIGHT0, GL_DIFFUSE, whiteLight)
		glLightModel(GL_LIGHT_MODEL_AMBIENT, lModelAmbient)

		glEnable(GL_LIGHTING)
		glEnable(GL_LIGHT0)
    glEnable(GL_COLOR_MATERIAL)
		glColorMaterial(GL_FRONT, GL_AMBIENT_AND_DIFFUSE)
	}
}

case class DisplayConf(val width: Int, val height: Int) {
  val resolution = Coord2i(width, height)
  val xfact = width / 1024d
  val yfact = height / 768d
/**
  val offscreenTex = new OffscreenTexture(width, height)

  def saveScreen() {
    glLoadIdentity()
    glBindTexture(GL_TEXTURE_2D, offscreenTex.id)
    glCopyTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, 0, 0, width, height)
  }
*/
  def cleanUp() {
    Display.destroy()
  }
}
