package priv.entity

import priv._
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL12._
import org.lwjgl.opengl.GL13._
import org.lwjgl.opengl.GL15._
import org.lwjgl.opengl.GL20._
import org.lwjgl.util.glu.GLU._
import java.util.concurrent.atomic._

object Entity {
  val stdspeed = 1 / 1000000f
  val nextId = new AtomicInteger()
}


trait Entity  {
  val creationTime = System.nanoTime
  val id = Entity.nextId

  def render(world: World)

  def clean()

  protected def deltaT(time: Long) = time - creationTime
  @inline def stdspeed = Entity.stdspeed

}

class TexAnim(
  val animationTextures: Array[Texture],
  val ratio: Float = 1 / 500f) {
  
  def this(path: String, x: Int, y: Int, w: Int, h: Int) = this(loadAnimation(path, x, y, w, h))

  val width = ratio * animationTextures(0).width
  val height = ratio * animationTextures(0).height
  //  println(animationTextures(0).width + "/" + animationTextures(0).height)

  def length = animationTextures.length

  def textureId(i: Int) = animationTextures(i).id

  var cursor = 0
  def ended = (cursor == length - 1)

  def clean() {
    animationTextures.foreach { tex =>
      glDeleteTextures(tex.id)
    }
  }
}
