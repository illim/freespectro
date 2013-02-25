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
  val grad :: width :: height :: cursor :: _= getUniformLocations("grads", "width", "height", "cursor")
  val fbuffer = GBufferUtils.createFloatBuffer(200)
  
  fillBuffer(fbuffer)
  used {
    glUniform2(grad, fbuffer)
    glUniform1f(width, texture.width)
    glUniform1f(height, texture.height)
  }

  private def fillBuffer(fbuffer: java.nio.FloatBuffer) {
    //useless cr*p
    def radial(cx: Int, cy: Int)(x: Int, y: Int, vx: Float, vy: Float) : (Float, Float)= {
      @inline def pow2(v: Int) = v * v

      val dist = pow2(x - cx) + pow2(y - cy)
      val fac = 2f / (1 + dist/10)
      (vx * fac, vy * fac)
    }

    val arr = Array.fill(100)(Utils.floatRand(math.Pi * 2))
    val rad = radial(5, 5) _
    for (i <- 0 until arr.length) {
      val y = i / 10
      val x = i - 10 * y
      val (gx, gy) = rad(x, y, math.cos(arr(i)).floatValue, math.sin(arr(i)).floatValue)
      fbuffer.put(gx)
      fbuffer.put(gy)
    }
    fbuffer.rewind()
  }
}
