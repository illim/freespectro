package priv.sp

import collection._
import priv._
import priv.util.ResourceCache

class SpWorld {
  val textures = new Textures
  val baseTextures = new BaseTextures(textures)
  val shaders = new Shaders
  
  def clean(){
    textures.clean()
    shaders.clean()
  }
}

class Textures extends ResourceCache[String, Texture] {
  import org.lwjgl.opengl.GL11._

  def load(path: String) = loadTexture(path)
  def clean() {
    resources.values.foreach(tex => glDeleteTextures(tex.id))
    resources.clear()
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

import org.lwjgl.opengl.GL20._
import priv.util._

trait Shader {
  var started = false
  def program: Int
  def start() {
    glUseProgram(program)
    started = true
  }

  def end() {
    assert(started)
    glUseProgram(0)
  }
}

class Shaders extends ResourceCache[String, Shader] {

  def load(path : String) = new BasicShader(path)

  def clean() {
    resources.values.foreach(res => glDeleteProgram(res.program))
  }

  class BasicShader(name: String) extends Shader {
    val (program, _, _) = GShader.createShaderProgram(
      Utils.codeFromResource(name + ".vert"),
      Utils.codeFromResource(name + ".frag")).left.map(error).right.toOption.get
  }
}