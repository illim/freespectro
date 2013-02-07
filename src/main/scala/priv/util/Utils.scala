package priv.util

object Utils {
  
}

class StateView[A](read : => A){
  def map[B](f : A => B) = new StateView(f(read))
  def get = read
}