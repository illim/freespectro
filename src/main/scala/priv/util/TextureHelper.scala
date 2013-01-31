package priv.util

trait TextureHelper {
  import org.lwjgl.opengl.GL11._

  def textureUp = 1f
  def textureDown = 0f
  def textureLeft = 0f
  def textureRight = 1f

  def drawTexture(texture: priv.Texture) {
    drawTexture(texture.id, texture.width, texture.height)
  }
  def drawTexture(texId: Int, w: Int, h: Int) {
    glBindTexture(GL_TEXTURE_2D, texId)
    glBegin(GL_QUADS)
    drawTexture(w, h)
    glEnd()
  }
  def drawTexture(w: Int, h: Int) {
    glTexCoord2f(textureRight, textureUp)
    glVertex2f(w, h)

    glTexCoord2f(textureLeft, textureUp)
    glVertex2f(0, h)

    glTexCoord2f(textureLeft, textureDown)
    glVertex2f(0, 0)

    glTexCoord2f(textureRight, textureDown)
    glVertex2f(w, 0)
  }
}