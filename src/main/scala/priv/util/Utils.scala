package priv.util

import java.io._
import java.nio._
import javax.swing._
import java.awt.event._
import java.util.concurrent._
import collection.JavaConversions._
import org.lwjgl.opengl._
import org.lwjgl.opengl.GL11._
import org.lwjgl.util.glu.GLU._
import javax.imageio.stream._
import scalaz._

object Utils {

  trait Bufferable[A] {
    def bufferData(target: Int, data: Array[A], usage: Int)
  }

  implicit val intBufferable = new Bufferable[Int] {
    def bufferData(target: Int, data: Array[Int], usage: Int) = {
      val buffer = GBufferUtils.createIntBuffer(data.size)
      buffer.put(data).rewind
      GL15.glBufferData(target, buffer, usage)
    }
  }

  implicit val floatBufferable = new Bufferable[Float] {
    def bufferData(target: Int, data: Array[Float], usage: Int) = {
      val buffer = GBufferUtils.createFloatBuffer(data.size)
      buffer.put(data).rewind
      GL15.glBufferData(target, buffer, usage)
    }
  }

  def bindArrayToBuffer[A](target: Int, data: Array[A])(implicit bb: Bufferable[A]): Int = {
    val iBuf = GBufferUtils.createIntBuffer(1)
    GL15.glGenBuffers(iBuf)
    val bufId = iBuf.get
    GL15.glBindBuffer(target, bufId)
    bb.bufferData(target, data, GL15.GL_STATIC_DRAW)
    bufId
  }

  def ext(name: String) = name.drop(name.length - 3).toUpperCase

  def codeFromFile(filename: String): Option[String] = {
    var code = ""
    var line = ""
    lazy val reader = new BufferedReader(new FileReader(filename))
    try {
      line = reader.readLine()
      while (line != null) {
        code += line + "\n"
        line = reader.readLine()
      }
      Some(code)
    } catch {
      case e: Throwable =>
        e.printStackTrace
        None
    } finally {
      reader.close()
    }
  }

  def codeFromResource(name: String): Option[String] = {
    var code = ""
    var line = ""
    val is = getClass.getClassLoader.getResourceAsStream(name)
    lazy val reader = new BufferedReader(new InputStreamReader(is))
    try {
      line = reader.readLine()
      while (line != null) {
        code += line + "\n"
        line = reader.readLine()
      }
      Some(code)
    } catch {
      case e: Throwable =>
        e.printStackTrace
        None
    } finally {
      reader.close()
    }
  }

  def floatRand[N](n: N)(implicit num: Numeric[N]) = {
    val fl = scala.util.Random.nextFloat
    if (fl == 0) fl else num.toFloat(n) * math.abs(1 / fl)
  }

  def thread(name : String)(f: => Unit) = new Thread(runnable(f), name).start()
  def runnable(f: => Unit) = new Runnable() { def run() = {
    try {
    f
    } catch { case t : Throwable =>
      t.printStackTrace
    }
  }  }

  def deleteThenRight[A](treeLoc : TreeLoc[A]) : Option[TreeLoc[A]] = {
    treeLoc.rights match {
      case Stream.cons(t, ts) => Some(TreeLoc.loc(t, Stream.empty, ts, treeLoc.parents))
      case _ => None
    }
  }

  def iterate[A](ite: java.util.Iterator[A])(f: A => Unit) {
    while (ite.hasNext) {
      f(ite.next)
    }
  }

  // swing
  def doInDispatch(f : => Unit) = SwingUtilities.invokeLater(runnable(f))
  def addBtn(name : String, target : java.awt.Container with ActionListener) = {
    val b = new JButton(name)
    b.setActionCommand(name)
    b.addActionListener(target)
    target.add(b)
    b
  }
  def alignX[A<: JComponent](component : A, alignment : Float = java.awt.Component.LEFT_ALIGNMENT) : A = {
    component.setAlignmentX(alignment)
    component
  }
  def createAction(name: String)(f: => Unit) : AbstractAction = {
    new AbstractAction(name) {
      def actionPerformed(e: ActionEvent){ f }
    }
  }
}

class TVar[A <: AnyRef](lock : AnyRef){
  private var holder = Option.empty[A]
  def set(a : A){
    holder = Some(a)
    lock.synchronized {
      lock.notifyAll()
    }
  }

  def get() : Option[A] = {
    lock.synchronized {
      lock.wait()
    }
    holder
  }
}

class RichExecutor {
  val executor = Executors.newSingleThreadExecutor
  var lock = new Object

  def execute(f : => Unit){ executor.submit(Utils.runnable(f)) }
  def releaseLock(){ // this can be very dangerous if it is released during ai(TODO make it safer)
    lock.synchronized {
      lock.notifyAll()
    }
    lock = new Object
  }

  // return none when surrendering for example
  def waitFor[A <: AnyRef](f : TVar[A] => Unit) : Option[A] = {
    val t = new TVar[A](lock)
    f(t)
    t.get()
  }

  def waitLock(f : AnyRef => Unit){
    f(lock)
    lock.synchronized {
      lock.wait()
    }
  }
}

trait ResourceCache[A, B] {
  import collection._

  val resources = mutable.Map.empty[A, B]

  def get(path: A): B = resources.getOrElseUpdate(path, {
    load(path)
  })
  def gets(path: A*): List[B] = path.map(get)(breakOut)
  def getOrElseUpdate[C <: B](path: A, create: A => C): C = resources.getOrElseUpdate(path, create(path)).asInstanceOf[C]
  def load(path: A): B
  def clean()
}

abstract class FieldUpdate[A](parent : Option[FieldUpdate[_]], getValue : => A) {
  var tid = 0
  var dirty = false
  var value = getValue

  def initNewUpdate(value : A) : this.type = {
    write(value)
    tid += 1
    this
  }

  def reinit() : this.type = {
    if (tid != parent.get.tid) {
      dirty = false
      value = getValue
      tid = parent.get.tid
    }
    this
  }

  def write(v : A){
    value = v
    setDirty()
  }

  def isDirty = parent.exists(_.tid == tid && dirty)
  private def setDirty(){
    parent.foreach{ p =>
      p.setDirty()
      dirty = true
    }
  }

}
