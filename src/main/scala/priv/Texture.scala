package priv

import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL12._
import java.nio.ByteBuffer
import java.nio.ByteOrder

trait Texture{
  def id : Int
  def height : Int
  def width : Int
  def bind()

  val size = Coord2i(width, height)
}

class SampleTexture(data : ByteBuffer, val width : Int, val height : Int, hasAlpha : Boolean, bytesPerPixel : Int) extends Texture {
  val buf = ByteBuffer.allocateDirect(4).order(ByteOrder.nativeOrder()).asIntBuffer()
  glGenTextures(buf)

  val id = buf.get(0)
  bind()

  def bind(){
    glBindTexture(GL_TEXTURE_2D, id)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR)
    val colorModel = if (bytesPerPixel == 1) GL_LUMINANCE else if (hasAlpha) GL_RGBA else GL_RGB
    glTexImage2D(GL_TEXTURE_2D, 0,colorModel, width, height, 0,colorModel, GL_UNSIGNED_BYTE, data)
  }

}

class OffscreenTexture(val width : Int, val height : Int) extends Texture {
  val bytesPerPixel = 3
  val data = ByteBuffer.allocateDirect(height * width * bytesPerPixel)
  val buf = ByteBuffer.allocateDirect(12).order(ByteOrder.nativeOrder()).asIntBuffer()
  glGenTextures(buf)
  glBindTexture(GL_TEXTURE_2D, buf.get(0))
  val id = buf.get(0)
  bind()

  def bind(){
   glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR)
   glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR)
   glTexImage2D(GL_TEXTURE_2D, 0,GL_RGB,width, height, 0, GL_RGB, GL_UNSIGNED_BYTE, data)
  }
}

