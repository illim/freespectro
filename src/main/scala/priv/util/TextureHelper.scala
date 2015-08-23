package priv.util

import priv.Coord2i

class TextureHelper {
  import org.lwjgl.opengl.GL11._

  def textureUp = 1f
  def textureDown = 0f
  def textureLeft = 0f
  def textureRight = 1f

  def draw(texture: priv.Texture) { draw(texture.id, texture.size) }
  def draw(texId: Int, c: Coord2i, flip: Boolean = false) {
    glBindTexture(GL_TEXTURE_2D, texId)
    glBegin(GL_QUADS)
    draw(c.x, c.y, flip)
    glEnd()
  }

  def draw(w: Int, h: Int, flip: Boolean) {
    val (tup, tdown) = if (flip) (textureDown, textureUp) else (textureUp, textureDown)
    glTexCoord2f(textureRight, tup)
    glVertex2f(w, h)

    glTexCoord2f(textureLeft, tup)
    glVertex2f(0, h)

    glTexCoord2f(textureLeft, tdown)
    glVertex2f(0, 0)

    glTexCoord2f(textureRight, tdown)
    glVertex2f(w, 0)
  }

  def drawAt(c: Coord2i, texId: Int, size: Coord2i) {
    glBindTexture(GL_TEXTURE_2D, texId)
    glBegin(GL_QUADS)
    glTexCoord2f(textureRight, textureUp)
    glVertex2f(c.x + size.x, c.y + size.y)

    glTexCoord2f(textureLeft, textureUp)
    glVertex2f(c.x, c.y + size.y)

    glTexCoord2f(textureLeft, textureDown)
    glVertex2f(c.x, c.y)

    glTexCoord2f(textureRight, textureDown)
    glVertex2f(c.x + size.x, c.y)
    glEnd()
  }
}
