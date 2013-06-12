package priv

import collection._
import org.lwjgl.opengl.GL20._
import org.lwjgl.opengl.GLContext
import priv.util._

object Shader {
  val isSupported = {
		val c = GLContext.getCapabilities()
		c.GL_ARB_shader_objects && c.GL_ARB_vertex_shader && c.GL_ARB_fragment_shader
	}
}

trait Shader {
  private var started = false
  def program: Int
  def begin() {
    glUseProgram(program)
    started = true
  }

  def used[A](f: => A) = {
    begin()
    val res = f
    end()
    res
  }

  def end() {
    require(started)
    glUseProgram(0)
    started = false
  }

  def getUniformLocations(names : String*) : List[Int] = names.map(getUniformLocation(_))(breakOut)
  def getUniformLocation(name: String) = {
    val res = glGetUniformLocation(program, name)
    require(res != -1, name + " doesn't exists")
    res
  }
}
