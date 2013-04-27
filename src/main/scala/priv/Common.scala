package priv

import scalaz._

object Coord2i {
  val zero = Coord2i(0, 0)
  val xL = Lens.lensu[Coord2i, Int]((c, v) => c.copy(x = v), _.x)
  val yL = Lens.lensu[Coord2i, Int]((c, v) => c.copy(y = v), _.y)

  class Proj(lens : Lens[Coord2i, Int], c : Coord2i){
    def *[N](fact : N)(implicit num : Numeric[N]) = lens.mod(x => (num.toFloat(fact) * x).intValue, c)
    def +[N](x : N)(implicit num : Numeric[N]) = lens.mod(v => (num.toFloat(x) + v).intValue, c)
    def -[N](x : N)(implicit num : Numeric[N]) = this.+(num.negate(x))
  }

  @inline def pow2(x : Int) = x * x

  def sqrDist(a : Coord2i, b :Coord2i) = {
    pow2(b.x - a.x) + pow2(b.y - a.y)
  }

  def dist(a : Coord2i, b :Coord2i) = math.sqrt(sqrDist(a, b))

  /**
   * for example, if one texture have to be displayed on a center coordinate
   * @param c the point we want to become the center
   */
  def recenter(c : Coord2i, size :Coord2i) = c - (size * 0.5)
}

case class Coord2i(x: Int, y: Int) {
  def +(c : Coord2i) = Coord2i(x+c.x, y+c.y)
  def -(c : Coord2i) = Coord2i(x-c.x, y-c.y)
  def *[N](fact : N)(implicit num : Numeric[N]) = Coord2i((num.toFloat(fact) * x).intValue, (num.toFloat(fact) * y).intValue)

  import Coord2i._
  def xProj = new Proj(xL, this)
  def yProj = new Proj(yL, this)
  def size = math.sqrt(pow2(x) + pow2(y))
}
