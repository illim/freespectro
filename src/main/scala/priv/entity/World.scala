package priv.entity

import java.util.concurrent.ConcurrentLinkedQueue
import org.lwjgl.opengl._
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL12._
import org.lwjgl.opengl.GL13._
import org.lwjgl.opengl.GL15._
import org.lwjgl.opengl.GL20._
import org.lwjgl.util.glu.GLU._

class World{
  var time : Long = 0

  val entities = new ConcurrentLinkedQueue[Entity]

  def unspawn(entity : Entity){
    entities.remove(entity)
  }

  def render(){    
    iterate(entities.iterator)(_.render(this))
  }

  def iterate[A](ite : java.util.Iterator[A])(f : A => Unit){
    while(ite.hasNext){
      f(ite.next)
    }
  }

  def tick(){
    time = System.nanoTime
  }
}
