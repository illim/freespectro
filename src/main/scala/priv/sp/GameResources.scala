package priv.sp

import java.util.concurrent._
import java.net._
import priv.util.Utils.iterate

// bullcrap :)

object GameResources {
  type Closable = { def close() }
}
import GameResources._

class GameResources {
  val sp = new SpWorld
  val aiExecutor = Executors.newSingleThreadExecutor

  val multi = new Resources
  val serverSocket = multi(new ClosableOne[ServerSocket])
  val clientSocket = multi(new ClosableOne[Socket])

  def release(){
    println("releasing resources")
    multi.release()
    sp.clean()
    aiExecutor.shutdown()
  }
}

class Resources {
  private val resources = new ConcurrentLinkedQueue[Resource]
  def apply[A <: Resource](x : A) : A = {
    resources.add(x)
    x
  }

  def release(){iterate(resources.iterator)(_.release())}
}

trait Resource {
  def release()
}

abstract class One[A] extends Resource {
  var x = Option.empty[A]
  def apply(v : => A) : A = {
    release()
    val a = v
    x = Some(a)
    a
  }
  def release(v : A)
  def release(){
    x.foreach(release _)
  }
}

class ClosableOne[A <: Closable] extends One[A] {
  def release(v : A) = v.close()
}