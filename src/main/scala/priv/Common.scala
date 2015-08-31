package priv


object Coord2i {
  val zero = Coord2i(0, 0)

  class Proj(read : Coord2i => Int, write : (Coord2i, Int) => Coord2i, c: Coord2i) {
    def *[N](fact: N)(implicit num: Numeric[N]) = write(c, (num.toFloat(fact) * read(c)).intValue)
    def +[N](x: N)(implicit num: Numeric[N]) = write(c, (num.toFloat(x) + read(c)).intValue)
    def -[N](x: N)(implicit num: Numeric[N]) = this.+(num negate x)
  }

  @inline def pow2[N](x: N)(implicit num: Numeric[N]) = num.times(x, x)

  def sqrDist(a: Coord2i, b: Coord2i) = {
    pow2(b.x - a.x) + pow2(b.y - a.y)
  }

  def dist(a: Coord2i, b: Coord2i) = math.sqrt(sqrDist(a, b))

  /**
   * for example, if one texture have to be displayed on a center coordinate
   * @param c the point we want to become the center
   */
  def recenter(c: Coord2i, size: Coord2i) = c - (size * 0.5)
}

case class Coord2i(x: Int, y: Int) {
  def +(c: Coord2i) = Coord2i(x + c.x, y + c.y)
  def -(c: Coord2i) = Coord2i(x - c.x, y - c.y)
  def *[N](fact: N)(implicit num: Numeric[N]) = Coord2i((num.toFloat(fact) * x).intValue, (num.toFloat(fact) * y).intValue)

  import Coord2i._
  def xProj = new Proj(_.x, (c : Coord2i, i : Int) => c.copy(x = i), this)
  def yProj = new Proj(_.y, (c : Coord2i, i : Int) => c.copy(y = i), this)
  def size = math.sqrt(pow2(x) + pow2(y))
}
