package priv.util

import java.nio.ByteBuffer
import java.nio.ByteOrder
import java.nio._
import org.newdawn.slick.util.ResourceLoader
import javax.imageio.ImageIO
import java.awt.image.BufferedImage
import javax.imageio.stream._

import java.io._
import priv.SampleTexture
import priv.Texture

trait TextureLoader {

  def loadTexture(name: String): Texture = {
    val buffImage = getBuffImage(name)

    val bytesPerPixel = buffImage.getColorModel().getPixelSize() / 8
    /**
     *   println(
     * "load " + name
     * + ", bpp " + bytesPerPixel
     * + ", height " + buffImage.getHeight
     * + ", width " + buffImage.getWidth
     * + ", type " + buffImage.getType)
     */
    val scratch = ByteBuffer.allocateDirect(buffImage.getWidth() * buffImage.getHeight() * bytesPerPixel).order(ByteOrder.nativeOrder())
    scratch.clear()
    scratch.put(buffImage.getData().getDataElements(0, 0, buffImage.getWidth(), buffImage.getHeight(), null).asInstanceOf[Array[Byte]])
    scratch.rewind()

    new SampleTexture(scratch, buffImage.getWidth(), buffImage.getHeight(), buffImage.getColorModel().hasAlpha(), bytesPerPixel)
  }

  def loadTexture(name: String, xOffSet: Int, yOffSet: Int, textWidth: Int, textHeight: Int): Texture = {
    val buffImage = getBuffImage(name)
    val bytesPerPixel = buffImage.getColorModel().getPixelSize() / 8
    /**
     * println(
     * "load "+name
     * +", bpp " + bytesPerPixel
     * + ", height "+ buffImage.getHeight
     * + ", width "+ buffImage.getWidth
     * + ", type "+ buffImage.getType
     * + ", x,y " + xOffSet +"/" + yOffSet)
     */
    val scratch = ByteBuffer.allocateDirect(textWidth * textHeight * bytesPerPixel).order(ByteOrder.nativeOrder())
    scratch.clear()
    scratch.put(buffImage.getRaster().getDataElements(xOffSet, yOffSet, textWidth, textHeight, null).asInstanceOf[Array[Byte]])
    scratch.rewind()

    new SampleTexture(scratch, textWidth, textHeight, buffImage.getColorModel().hasAlpha(), bytesPerPixel)
  }

  def loadSamples(name: String, offsets: List[(Int, Int)], textWidth: Int, textHeight: Int): List[Texture] = {
    val buffImage = getBuffImage(name)
    val bytesPerPixel = buffImage.getColorModel().getPixelSize() / 8
    offsets.map {
      case (xOffSet, yOffSet) ⇒
        val scratch = ByteBuffer.allocateDirect(textWidth * textHeight * bytesPerPixel).order(ByteOrder.nativeOrder())
        scratch.clear()
        scratch.put(buffImage.getRaster().getDataElements(xOffSet, yOffSet, textWidth, textHeight, null).asInstanceOf[Array[Byte]])
        scratch.rewind()

        new SampleTexture(scratch, textWidth, textHeight, buffImage.getColorModel().hasAlpha(), bytesPerPixel)
    }
  }

  def loadAnimation(path: String, cols: Int, rows: Int, textWidth: Int, textHeight: Int): Array[Texture] = {
    val toReturntextures = new Array[Texture](cols * rows)

    for {
      i ← 0 until rows
      j ← 0 until cols
    } toReturntextures(i * cols + j) = loadTexture(path, j * textWidth, i * textHeight, textWidth, textHeight)

    toReturntextures
  }

  private def getBuffImage(name: String) = {
    if (name.endsWith("tga")) {
      TargaReader.getImage(ResourceLoader.getResourceAsStream(name))
    } else {
      ImageIO.read(new MemoryCacheImageInputStream(ResourceLoader.getResourceAsStream(name)))
    }
  }
}

object TargaReader {

  def getImage(is: InputStream): BufferedImage = {
    decode(new DataInputStream(new BufferedInputStream(is)))
  }

  def decode(is: DataInputStream): BufferedImage = {
    def hilo(s: Int) = (s >> 8) + (s << 8 & 0xFF00)
    val identificationField = is.readByte()
    val colorMapType = is.readByte()
    val imageType = is.readByte()
    val cmapOrigin = hilo(is.readUnsignedShort())
    val cmapLength = hilo(is.readUnsignedShort())
    val cmapEntrySize = is.readByte()
    val xcoord = hilo(is.readUnsignedShort())
    val ycoord = hilo(is.readUnsignedShort())
    val width = hilo(is.readUnsignedShort())
    val height = hilo(is.readUnsignedShort())
    val pixelSize = is.readByte()
    val descriptor = is.readByte()
    assert(identificationField == 0x00) // TODO

    var n = width * height
    val bimg = new BufferedImage(width, height, BufferedImage.TYPE_4BYTE_ABGR)
    val pixels = new Array[Byte](n * 4)
    var i = 0
    var j = height - 1
    val bwidth = width * 4
    var k = j * bwidth

    if (imageType == 0x01 && pixelSize == 0x08) {
      // TODO use indexcolormodel
      val crangelength = cmapEntrySize / 8
      val cbrange = 1 to crangelength
      val cmap = (1 to cmapLength).map { _ ⇒
        cbrange.map(_ ⇒ is.readByte())
      }
      while (n > 0) {
        val c = is.readUnsignedByte()
        pixels(i + k) = cmap(c)(2)
        pixels(i + k + 1) = cmap(c)(1)
        pixels(i + k + 2) = cmap(c)(0)
        pixels(i + k + 3) = if (crangelength == 4) cmap(c)(3) else 0
        i += 4;
        if (i >= bwidth) {
          i = 0;
          j -= 1;
          k = j * bwidth;
        }
        n -= 1;
      }
    } else if (imageType == 0x02 && pixelSize == 0x20) {
      // BGRA

      while (n > 0) {
        val b = is.readByte()
        val g = is.readByte()
        val r = is.readByte()
        val a = is.readByte()
        pixels(i + k) = r
        pixels(i + k + 1) = g
        pixels(i + k + 2) = b
        pixels(i + k + 3) = a
        i += 4;
        if (i >= bwidth) {
          i = 0;
          j -= 1;
          k = j * bwidth;
        }
        n -= 1;
      }
    } else {
      sys.error("format not managed " + imageType + ", " + pixelSize + ", " + cmapEntrySize)
    }
    bimg.getRaster().setDataElements(0, 0, width, height, pixels)
    bimg
  }

}
