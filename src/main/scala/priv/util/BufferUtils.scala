package priv.util

import java.nio.{ByteBuffer,IntBuffer,FloatBuffer}
import java.nio.ByteOrder

object GBufferUtils {

  final def packedIntArrayToByteArray(array : Array[Int]) : Array[Byte] = {
    val numInts = array.length
    val dst = new Array[Byte](numInts*4)
    var i = 0
    while (i < numInts) {
      val packed32 = array(i)
      dst(4*i  ) = ((packed32>>> 0)&0xFF).toByte
      dst(4*i+1) = ((packed32>>> 8)&0xFF).toByte
      dst(4*i+2) = ((packed32>>>16)&0xFF).toByte
      dst(4*i+3) = ((packed32>>>24)&0xFF).toByte
      i += 1
    }
    dst
  }

  final def byteArrayToByteBuffer(src : Array[Byte]) : ByteBuffer = {
    val size = src.length
    val dst = createByteBuffer(size)
    dst.put(src, 0, size).rewind
    dst
  }

  final def byteArrayToByteBuffer(src : Array[Byte], dst : ByteBuffer) : ByteBuffer = {
    val size = src.length
    val position = dst.position
    dst.put(src, 0, size)
    dst.position(position)
    dst
  }

  final def byteBufferToByteArray(buff : ByteBuffer) : Array[Byte] = {
    if (buff != null) {
      val size = buff.remaining()
      val array = new	Array[Byte](size)
      buff.get(array, 0, size)
      buff.rewind()
      array
    } else null
  }

  final def byteBufferToByteArray(src : ByteBuffer,dst : Array[Byte]) :  Array[Byte] = {
    val position = src.position
    src.get(dst, 0, src.remaining())
    src.position(position)
    dst
  }


  val floatSize = 4
  val intSize = (Integer.SIZE/8)

  final def createFloatBuffer(size : Int) : FloatBuffer = {
    createByteBuffer(size * floatSize).asFloatBuffer()
  }
  final def createIntBuffer(size : Int) : IntBuffer = {
    createByteBuffer(size * intSize).asIntBuffer()
  }
  final def createByteBuffer(size : Int) : ByteBuffer = {
    val back = ByteBuffer.allocateDirect(size).order(ByteOrder.nativeOrder())
    back.clear()
    back
  }


  final def slice(buffer : ByteBuffer) : ByteBuffer = {
    val newBuffer = buffer.slice
    newBuffer.order(ByteOrder.nativeOrder())
    newBuffer
  }

  final def slice(buffer : ByteBuffer, pos : Int, size : Int) = {
    var oldPos = buffer.position
    buffer.position(pos)
    val newBuffer = buffer.slice
    newBuffer.limit(size)
    newBuffer.order(ByteOrder.nativeOrder())
    buffer.position(oldPos)
    newBuffer
  }
}
