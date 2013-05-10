package priv.sp

import collection._
import priv._
import priv.util.ResourceCache

class SpWorld {
  val textures = new Textures
  val baseTextures = new BaseTextures(textures)
  val shaders = new Shaders
  val baseShaders = new BaseShaders(shaders, this)
  val houses : Houses = HouseSingleton

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
  val borderTex :: borderTexSpell :: cardGlow :: slotTex :: blank :: _ = textures.gets(
    "Images/Combat/raka.tga",
    "Images/Combat/rakaSpell.tga",
    "Images/Combat/glow.tga",
    "Images/Combat/slot.tga",
    "blank.png")

  val parts = loadSamples("Images/Combat/parts.tga", (0 to 5).map( i => (i * 32, 0)).toList, 32, 32)
  val fire = parts(0)
  val stones = parts.slice(1, 3)
  val natureLight = parts(4)
  val pollen = parts(5)

  def getBorder(card: Card) = if (card.isSpell) borderTexSpell else borderTex
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
  def selectedGlow(name : String, s : Int) = shaders.getOrElseUpdate("sel"+name, _ => new SelectedShader("sel", s))
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

class SelectedShader(name: String, s : Int) extends Shader {
  val (program, _, _) = GShader.createProgram(name, name)
  val size :: cursor :: offset :: _= getUniformLocations("size", "cursor", "offset")

  used {
    glUniform1f(size, s)
  }
}
