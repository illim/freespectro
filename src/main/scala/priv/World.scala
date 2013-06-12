package priv

import java.util.concurrent._
import org.lwjgl.opengl._
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL12._
import org.lwjgl.opengl.GL13._
import org.lwjgl.opengl.GL15._
import org.lwjgl.opengl.GL20._
import org.lwjgl.util.glu.GLU._
import priv.util.Utils.iterate

class World(displayConf : DisplayConf) extends Attachable {
  var time: Long = 0
  var ended = false
  val resolution = displayConf.resolution
  setWorld(this)

  def tick() {
    time = System.currentTimeMillis
  }
}

trait Attachable extends Entity {
  private val entities = new ConcurrentLinkedQueue[Entity]
  private val tasks = new ConcurrentLinkedQueue[Task[_]]
  private var shaderOpt = Option.empty[ShaderEntity]

  def spawn(entity: Entity) {
    entity.setWorld(world)
    entity match {
      case s : ShaderEntity => shaderOpt = Some(s)
      case _ => entities.add(entity)
    }
  }

  def unspawn(entity: Entity) {
    entity match {
      case s : ShaderEntity => shaderOpt = None
      case _ => entities.remove(entity)
    }
  }
  override def setWorld(w : World){
    super.setWorld(w)
    iterate(entities.iterator)(_.setWorld(w))
    shaderOpt.foreach(_.setWorld(w))
  }

  def render() {
    val shader = shaderOpt.map{ s =>
      s.shader.begin()
      s.render()
      s.shader
    }
    iterate(tasks.iterator()) { task =>
      if (world.time - task.start > task.duration) {
        tasks.remove(task)
        task.finish()
      }
    }
    iterate(entities.iterator)(_.render())
    shader.foreach(_.end())
  }

  def forEntity[A : reflect.ClassTag](f : A => Unit){
    var ls = List.empty[A]
    iterate(entities.iterator){
      case elem : A => ls = elem :: ls
      case _ =>
    }
    ls.foreach(f)
  }

  def clear(){
    entities.clear()
    tasks.clear()
    shaderOpt.foreach(_.shader.end())
    shaderOpt = None
  }

  def addTask(task: Task[_]) {
    task.setWorld(world)
    task.start = System.currentTimeMillis
    task.init()
    tasks.add(task)
  }

  def doInRenderThread(f: => Unit) = addTask(new SimpleTask(f))
}

object Entity {
  val stdspeed = 1 / 1000f
  val lastId = new atomic.AtomicInteger()
}

trait Entity {
  var creationTime = System.currentTimeMillis
  val id = Entity.lastId.incrementAndGet()
  protected var world : World = null

  def render()

  def setWorld(w : World){  world = w }
  protected def getDelta() = world.time - creationTime
  @inline def stdspeed = Entity.stdspeed
  override def hashCode() = id
}

class SimpleEntity(f : => Unit) extends Entity {
  def render() = f
}

trait TimedEntity extends Entity {
  def duration : Long
  def onEnd(){}
}

trait ShaderEntity extends Entity {
  type S <: Shader
  def shader : S
}

trait Task[A] {
  var start = 0L
  var result = Option.empty[A]
  var cont = List.empty[() => Unit]
  protected var world : World = null

  def duration: Long
  def init()
  def end() : A
  def setWorld(w : World){  world = w }
  private[priv] def finish(){
    result = Some(end())
    cont.foreach(_())
  }
}

class SimpleTask(f : => Unit) extends Task[Unit]{
  val duration = 0L
  def init(){}
  def end(){f}
}

class BlockingTask(f : => Unit, lock : AnyRef) extends Task[Unit]{
  val duration = 0L
  def init(){}
  def end(){
    f
    lock.synchronized(lock.notifyAll())
  }
}

case class TaskSpawn(entity : TimedEntity, lockOption : Option[AnyRef] = None) extends Task[Unit]{
  val duration = entity.duration
  def init(){
    entity.creationTime = start
    world.spawn(entity)
  }
  def end(){
    world.unspawn(entity)
    entity.onEnd()
    lockOption.foreach{ lock =>
      lock.synchronized(lock.notifyAll())
    }
  }
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
