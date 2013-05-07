package priv.util

import oscar.cp.modeling._
import oscar.search._
import oscar.cp.core._
import scala.util.Random

object CpHelper {

  // todo why _.randomValue not work?
  def getRandom(x : CPVarInt) = {
    val ind = Random.nextInt(x.size)
    x.toArray.apply(ind)
  }

  val MAXRETRY = 3
  def softRun(cp : CPSolver, timeLimit : Int = Int.MaxValue) = {
    if (timeLimit != Int.MaxValue){
      cp.run(1, timeLimit = timeLimit)
    } else {
      var failed = true
      var i = 0
      while(failed && i < MAXRETRY){
        cp.run(1, timeLimit = 1 + i)
        failed = cp.isFailed
      }
      if (cp.isFailed){
        println("last retry")
        cp.run(1, timeLimit = Int.MaxValue)
      }
    }
  }
}
trait CpHelper {
  def cp : CPSolver

  def contains(x : Int, l : Traversable[CPVarInt]) = {
    l.foldLeft(CPVarBool(cp, false)){ (acc, e) =>
      acc || (e === x)
    }
  }

  def notContains(x : Int, l : Traversable[CPVarInt]) = {
    l.foldLeft(CPVarBool(cp, true)){ (acc, e) =>
      acc && (e !== x)
    }
  }
}
