package priv.util

import priv._
import collection.JavaConversions._
import org.lwjgl.opengl._
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL12._
import org.lwjgl.opengl.GL13._
import org.lwjgl.opengl.GL15._
import org.lwjgl.opengl.GL20._
import org.lwjgl.util.glu.GLU._

object Repere extends Entity {

  def render(world: World) {
glDisable(GL_TEXTURE_2D)
    glPolygonMode(GL_FRONT_AND_BACK, GL_LINE)
    glBegin(GL_LINES)

    glColor4f(1f, 0f, 0.0f, 1f)
    glVertex2f(1f, 1f)
    glVertex2f(100f, 1f)

    glColor4f(0.0f, 1f, 00f, 1f)
    glVertex2f(1f, 1f)
    glVertex2f(1f, 100f)

    glColor4f(0.0f, 0f, 1f, 1f)
    glVertex2f(1f, 1f)
    glVertex3f(1f, 1f, 100f)

    glEnd()
    glClearColor(0f, 0f, 0f, 0.0f)
    glPolygonMode(GL_FRONT_AND_BACK, GL_FILL)
    glEnable(GL_TEXTURE_2D)
  }
}
