package priv

import collection._
import org.lwjgl.opengl.GL20._
import priv.util._

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
    assert(started)
    glUseProgram(0)
    started = false
  }
  
  def getUniformLocations(names : String*) : List[Int] = names.map(getUniformLocation(_))(breakOut)
  def getUniformLocation(name: String) = {
    val res = glGetUniformLocation(program, name)
    assert(res != -1, name + " doesn't exists")
    res
  }
}