package priv.sp

class SpWorld {
  val textures = new Textures
  val baseTextures = new BaseTextures(textures)
}

class Textures {
  import collection._
  import priv._
  import org.lwjgl.opengl.GL11._

  val textures = mutable.Map.empty[String, Texture]
  def get(path: String): Texture = textures.getOrElseUpdate(path, {
    loadTexture(path)
  })
  def gets(path: String*): List[Texture] = path.map(get)(breakOut)

  def clean() {
    textures.values.foreach(tex => glDeleteTextures(tex.id))
    textures.clear()
  }
}
class BaseTextures(textures: Textures) {
  val borderTex :: borderTexSpell :: maskTex :: cardGlow :: slotTex :: _ = textures.gets(
    "Images/Combat/ramka.tga",
    "Images/Combat/ramkaSpell.tga",
    "Images/Combat/spellmask.tga",
    "Images/Combat/cardglow.tga",
    "Images/Combat/bottomslot.tga")

  def getBorder(card: Card) = if (card.isSpell) (borderTexSpell, maskTex) else (borderTex, maskTex)

}
