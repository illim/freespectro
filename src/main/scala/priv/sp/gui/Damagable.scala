package priv.sp.gui

import priv._
import org.lwjgl.opengl.GL11._
import priv.sp._


trait Damagable {

  var getDamageAnimOpt = Option.empty[DamageAnimTask]

  case class DamageAnimTask(damage : Int) extends Task[Unit] {
    val duration = 1000L
    val text = if (damage > 0) "+"+damage else damage
    val color = if (damage > 0) 'green else 'red
    private val amplitude = 2
    def init() { getDamageAnimOpt = Some(this) }
    def end() = { getDamageAnimOpt = None }
    def delta(time: Long) = (amplitude * (time - start) / 100).intValue
  }
}

class DamagableInt(getValue : => Int, world : World) extends Damagable {
  def this(getValue : => Int, game : Game) = this(getValue, game.world)
  var current = getValue

  def refresh(silent : Boolean = false) {
    val old = current
    current = getValue
    val d = current - old
    if (d != 0 && !silent){
      world.addTask(DamageAnimTask(d))
    }
  }
}
