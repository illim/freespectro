package priv.sp.house

import priv.sp._
import priv.sp.update._
import CardSpec._
import GameCardEffect._

case class AttackAdd(bonus : Int) extends AttackFunc { def apply(attack : Int) = attack + bonus }

class RemoveAttack(attack : AttackSource) extends Function[Env, Unit]{
  def apply(env : Env){
    env.player.slots.foreach(_.attack.removeFirst(attack))
    env.player.removeEffect(_.isInstanceOf[RemoveAttack])
  }
}

case class AttackFactor(fact : Float) extends AttackFunc {
  def apply(attack : Int) : Int = math.ceil(attack * fact).toInt
}

case object SkipTurn extends DescMod {
  def apply(house : House, cards : Vector[CardDesc]) : Vector[CardDesc] = {
    cards.map(c => c.copy( enabled = false ))
  }
}
