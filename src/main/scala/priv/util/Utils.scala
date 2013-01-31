package priv.util

object Utils {

}

class Once {
  var isSet = false
  def apply(f: => Unit) {
    if (!isSet) {
      f
      isSet = true
    }
  }
}