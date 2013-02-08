package priv

import java.util.concurrent._
import org.lwjgl.opengl._
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL12._
import org.lwjgl.opengl.GL13._
import org.lwjgl.opengl.GL15._
import org.lwjgl.opengl.GL20._
import org.lwjgl.util.glu.GLU._

class World {
  var time: Long = 0
  var ended = false

  val entities = new ConcurrentLinkedQueue[Entity]
  private val tasks = new ConcurrentLinkedQueue[Task[_]]

  def unspawn(entity: Entity) {
    entities.remove(entity)
  }

  def render() {
    iterate(entities.iterator)(_.render(this))
    iterate(tasks.iterator()) { task =>
      if (time - task.start > task.duration) {
        tasks.remove(task)
        task.finish()
      }
    }
  }

  def iterate[A](ite: java.util.Iterator[A])(f: A => Unit) {
    while (ite.hasNext) {
      f(ite.next)
    }
  }

  def addTask(task: Task[_]) {
    task.start = time
    task.init()
    tasks.add(task)
  }

  def tick() {
    time = System.currentTimeMillis
  }

}

object Entity {
  val stdspeed = 1 / 1000f
  val nextId = new atomic.AtomicInteger()
}

trait Entity {
  val creationTime = System.currentTimeMillis
  val id = Entity.nextId

  def render(world: World)

  protected def deltaT(time: Long) = time - creationTime
  @inline def stdspeed = Entity.stdspeed

}

object Task {
  
  import scala.util.continuations._
  def chain[A](world : World, tasks: Iterable[Task[A]]) = shift { k: (Unit => Unit) =>
    if (tasks.isEmpty) k()
    else world.addTask(new TaskChain(world, tasks, k))
  }
  private class TaskChain[A](world : World, tasks: Iterable[Task[A]], k : Unit => Unit) extends Task[Unit] {
    val ite = tasks.iterator
    var current = ite.next
    val duration = current.duration

    def init() { world.addTask(current) }
    def end() {
      if (ite.hasNext) {
        current = ite.next
        world.addTask(this)
      } else {
        k()
      }
    }
  }
}

trait Task[A] {
  var start = 0L
  var result = Option.empty[A]
  def duration: Long
  def init()
  def end() : A
  private[priv] def finish(){result = Some(end())}
}

class TexAnim(
  val animationTextures: Array[Texture],
  val ratio: Float = 1 / 500f) {

  def this(path: String, x: Int, y: Int, w: Int, h: Int) = this(loadAnimation(path, x, y, w, h))

  val width = ratio * animationTextures(0).width
  val height = ratio * animationTextures(0).height
  //  println(animationTextures(0).width + "/" + animationTextures(0).height)

  def length = animationTextures.length

  def textureId(i: Int) = animationTextures(i).id

  var cursor = 0
  def ended = (cursor == length - 1)

  def clean() {
    animationTextures.foreach { tex =>
      glDeleteTextures(tex.id)
    }
  }
}
