package priv.sp

import collection._
import priv._
import priv.util.ResourceCache

class SpWorld {
  val textures = new Textures
  val baseTextures = new BaseTextures(textures)
  val shaders = new Shaders
  val baseShaders = new BaseShaders(shaders, this)

  def clean() {
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
  val borderTex :: borderTexSpell :: maskTex :: cardGlow :: slotTex :: blank :: _ = textures.gets(
    "Images/Combat/ramka.tga",
    "Images/Combat/ramkaSpell.tga",
    "Images/Combat/spellmask.tga",
    "Images/Combat/cardglow.tga",
    "Images/Combat/bottomslot.tga",
    "blank.png")

  def getBorder(card: Card) = if (card.isSpell) (borderTexSpell, maskTex) else (borderTex, maskTex)

}

import org.lwjgl.opengl.GL20._
import priv.util._

trait Shader {
  private var started = false
  def program: Int
  def begin() {
    glUseProgram(program)
    started = true
  }

  def used[A](f: => A) = {
    begin()
    val res = f
    end()
    res
  }

  def end() {
    assert(started)
    glUseProgram(0)
    started = false
  }
}

class Shaders extends ResourceCache[String, Shader] {

  def load(path: String) = new BasicShader(path)

  def clean() {
    resources.values.foreach(res => glDeleteProgram(res.program))
  }

  class BasicShader(name: String) extends Shader {
    val (program, _, _) = GShader.createProgram(name, name)
  }
}

class BaseShaders(shaders: Shaders, sp: SpWorld) {

  val hoverGlow = shaders.getOrElseUpdate("hoverglow", _ => new HoverShader("nz", sp.baseTextures.cardGlow))
}

class HoverShader(name: String, texture: Texture) extends Shader {
  val (program, _, _) = GShader.createProgram(name, name)

  val grad = GShader.getUniformLocation(program, "grads")
  val width = GShader.getUniformLocation(program, "width")
  val height = GShader.getUniformLocation(program, "height")
  val fbuffer = GBufferUtils.createFloatBuffer(200)
  Array.fill(100)(Utils.floatRand(math.Pi * 2)).foreach { a =>
    fbuffer.put(math.cos(a).floatValue)
    fbuffer.put(math.sin(a).floatValue)
  }
  fbuffer.rewind()
  val cursor = GShader.getUniformLocation(program, "cursor")
  used {
    glUniform2(grad, fbuffer)
    glUniform1f(width, texture.width)
    glUniform1f(height, texture.height)
  }

}