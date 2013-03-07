package priv


object Coord2i {
  val zero = Coord2i(0, 0)
}

case class Coord2i(x: Int, y: Int) {
  def +(c : Coord2i) = Coord2i(x+c.x, y+c.y)
}
