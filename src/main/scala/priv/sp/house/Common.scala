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

trait OwnerDeathEventListener { _ : HouseEventListener =>
  override def onDeath(dead : Dead) {
    if (dead.player.id == player.id){
      player.slots.foreach{ s =>
        if (s.num != dead.num) {
          val card = s.get.card
          if (card.isSpecial){
            card.reaction.onDeath(s.num, player.id, dead)
          }
        }
      }
    }
  }
}

trait AnyDeathEventListener { _ : HouseEventListener =>
  override def onDeath(dead : Dead) {
    player.slots.foreach{ s =>
      if (dead.player.id != player.id || s.num != dead.num){
        val card = s.get.card
        if (card.isSpecial){
          card.reaction.onDeath(s.num, player.id, dead)
        }
      }
    }
  }
}


trait DamageAttack {

  // return amount of damage done
  def damageAndGet(num : Int, d : Damage, player : PlayerUpdate) = {
    val otherPlayer = player.otherPlayer
    val slot = otherPlayer.slots(num)
    slot.value match {
      case None =>
        val oldl = otherPlayer.value.life
        otherPlayer.inflict(d, Some(SlotSource(player.id, num)))
        oldl - otherPlayer.value.life
      case Some(slotState) =>
        val oldl = slotState.life
        slot.inflict(d)
        val newl = slot.value.map(_.life) getOrElse 0
        oldl - newl
    }
  }
}
