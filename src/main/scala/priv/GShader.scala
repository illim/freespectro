package priv

import org.lwjgl.opengl.GL11
import java.nio.ByteBuffer
import java.nio.IntBuffer
import org.lwjgl.BufferUtils
import org.lwjgl.opengl.ARBShaderObjects
import org.lwjgl.opengl.ARBVertexShader
import org.lwjgl.opengl.ARBFragmentShader
import org.lwjgl.opengl.Util
import collection.JavaConversions._
import java.nio.ByteBuffer
import java.nio.ByteOrder
import java.nio.IntBuffer
import org.lwjgl.LWJGLException
import org.lwjgl.input.Keyboard
import org.lwjgl.opengl._
import org.lwjgl.util.vector.Vector2f
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL20._
import org.lwjgl.util.glu.GLU._

object GShader {

  def createShaderProgram(vertCode : => Option[String], fragCode : => Option[String]) : Either[String, (Int, Int, Int)] = {
    val shader = glCreateProgram()
    if (shader != 0) {
      val v = createVertShader(vertCode _)
      val f = createFragShader(fragCode _)
      if (v * f != 0) {
        glAttachShader(shader, v)
        glAttachShader(shader, f)
        glLinkProgram(shader)
        glValidateProgram(shader)
        if (printLogInfo(shader)){
          Right((shader, v, f))
        } else Left("Error in shader log")
      } else Left("vert/frag shader " + v + ", " + f)
    } else Left("shader " + shader)
  }

  def usingShader[A](shader : Int)(f : => A) = {
    glUseProgram(shader)
    try {
      f
    } finally glUseProgram(0)
  }

  val createVertShader = loadAndCheckObject(GL_VERTEX_SHADER)
  val createFragShader = loadAndCheckObject(GL_FRAGMENT_SHADER)

  def loadAndCheckObject(shaderType : Int) = { (getCode : () => Option[String]) =>
    val shader = glCreateShader(shaderType)
    if(shader==0){
      0
    } else {
      getCode().map{ code =>
        glShaderSource(shader, code)
        glCompileShader(shader)
        if (glGetShader(shader, GL_COMPILE_STATUS) == GL11.GL_FALSE){
          println("err in shader")
        }
        if(printShaderLogInfo(shader)){
          shader
        } else {
          glDeleteShader(shader)
          0
        }
      }.getOrElse(0)
    }
  }

  def printShaderLogInfo(obj : Int) = {
    val log = glGetShaderInfoLog(obj, 65536)
    if (log.length() != 0) {
      System.err.println("Program link log:\n" + log)
    }
    log.length() == 0
  }

  def printLogInfo(obj : Int) = {
    val log = glGetProgramInfoLog(obj, 65536)
    if (log.length() != 0) {
      System.err.println("Program link log:\n" + log)
    }
    log.length() == 0
  }

  def debugAttributes(program : Int){
    val numAttributes = glGetProgram(program, GL_ACTIVE_ATTRIBUTES)
    val maxAttributeLength = glGetProgram(program, GL_ACTIVE_ATTRIBUTE_MAX_LENGTH)
    for (i <- 0 until numAttributes) {
      val name = glGetActiveAttrib(program, i, maxAttributeLength)
      val location = glGetAttribLocation(program, name)
      println(name + ":" + location)
        //attributeLocations.put(name, location)
    }
  }

  def debugUniforms(program : Int){
    val numUniforms = glGetProgram(program, GL_ACTIVE_UNIFORMS)
    val maxUniformLength = glGetProgram(program, GL_ACTIVE_UNIFORM_MAX_LENGTH)
    for (i <- 0 until numUniforms) {
      val name = glGetActiveUniform(program, i, maxUniformLength)
      val location = glGetUniformLocation(program, name)
//        uniformLocations.put(name, location)
      println(name + ":" + location)
    }
  }
}
