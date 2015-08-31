package priv.sp.gui

import priv._
import priv.sp._

case class Location(init: Coord2i) {

  var c = init

  def reset() {
    c = init
  }
}

case class Running(location: Location, direction: Int) extends TimedEntity {
  val duration = 1200L
  private val half = duration / 2
  private val amplitude = 2

  def render() {
    location.c = location.c.copy(y =
      location.init.y + (amplitude * direction * (half - math.abs(half - getDelta())) / 100).toInt)
  }

  override def onEnd() { location.reset() }
}
