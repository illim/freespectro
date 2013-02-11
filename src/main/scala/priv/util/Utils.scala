package priv.util

import java.io._
import collection.JavaConversions._
import java.nio._
import org.lwjgl.opengl._
import org.lwjgl.opengl.GL11._
import org.lwjgl.util.glu.GLU._
import javax.imageio.stream._

object Utils {
  
  trait Bufferable[A] {
    def bufferData(target: Int, data: Array[A], usage: Int)
  }

  implicit val intBufferable = new Bufferable[Int] {
    def bufferData(target: Int, data: Array[Int], usage: Int) = {
      val buffer = GBufferUtils.createIntBuffer(data.size)
      buffer.put(data).rewind
      GL15.glBufferData(target, buffer, usage)
    }
  }

  implicit val floatBufferable = new Bufferable[Float] {
    def bufferData(target: Int, data: Array[Float], usage: Int) = {
      val buffer = GBufferUtils.createFloatBuffer(data.size)
      buffer.put(data).rewind
      GL15.glBufferData(target, buffer, usage)
    }
  }

  def bindArrayToBuffer[A](target: Int, data: Array[A])(implicit bb: Bufferable[A]): Int = {
    val iBuf = GBufferUtils.createIntBuffer(1)
    GL15.glGenBuffers(iBuf)
    val bufId = iBuf.get
    GL15.glBindBuffer(target, bufId)
    bb.bufferData(target, data, GL15.GL_STATIC_DRAW)
    bufId
  }

  def ext(name: String) = name.drop(name.length - 3).toUpperCase

  def codeFromFile(filename: String): Option[String] = {
    var code = ""
    var line = ""
    lazy val reader = new BufferedReader(new FileReader(filename))
    try {
      line = reader.readLine()
      while (line != null) {
        code += line + "\n"
        line = reader.readLine()
      }
      Some(code)
    } catch {
      case e : Throwable =>
        e.printStackTrace
        None
    } finally {
      reader.close()
    }
  }

  def codeFromResource(name: String): Option[String] = {
    var code = ""
    var line = ""
    val is = getClass.getClassLoader.getResourceAsStream(name)
    lazy val reader = new BufferedReader(new InputStreamReader(is))
    try {
      line = reader.readLine()
      while (line != null) {
        code += line + "\n"
        line = reader.readLine()
      }
      Some(code)
    } catch {
      case e : Throwable  =>
        e.printStackTrace
        None
    } finally {
      reader.close()
    }
  }
}

class StateView[A](read : => A){
  def map[B](f : A => B) = new StateView(f(read))
  def get = read
}


trait ResourceCache[A, B] {
  import collection._

  val resources = mutable.Map.empty[A, B]
  def get(path: A): B = resources.getOrElseUpdate(path, {
    load(path)
  })
  def gets(path: A*): List[B] = path.map(get)(breakOut)

  def load(path: A): B
  def clean()
}
