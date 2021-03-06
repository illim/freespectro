
package priv.sp.house

import priv.sp._
import priv.sp.update._
import CardSpec._
import GameCardEffect._

case object OneAttackBonus extends AttackFunc { def apply(attack: Int) = attack + 1 }
case class AttackAdd(bonus: Int) extends AttackFunc { def apply(attack: Int) = attack + bonus }

class RemoveAttack(attack: AttackSource) extends Function[Env, Unit] {
  def apply(env: Env) {
    env.player.slots foreach (_.attack.removeFirst(attack))
    env.player removeEffect (_ == this)
  }
}

case class RemoveInvincible(slotId: Int) extends Function[Env, Unit] {
  def apply(env: Env) {
    env.player.slots.slots.find(s ⇒ s.value.isDefined && s.get.id == slotId) foreach { s ⇒
      s.toggleOff(CardSpec.invincibleFlag)
    }
    env.player removeEffect (_ == this)
  }
}

// hack for warp
trait UniqueAttack

//crap
case class UnMod(mod: DescMod) extends Function[Env, Unit] {
  def apply(env: Env) {
    env.player removeDescMod mod
  }
}

case object HideBasicMod extends DescMod {
  def apply(house: House, cards: Vector[CardDesc]): Vector[CardDesc] = {
    if (house.houseIndex == 4) cards
    else cards.map(_.copy(enabled = false))
  }
}

case object HideSpecialMod extends DescMod {
  def apply(house: House, cards: Vector[CardDesc]): Vector[CardDesc] = {
    if (house.houseIndex != 4) cards
    else cards.map(_.copy(enabled = false))
  }
}

case object HideSpellMod extends DescMod {
  def apply(house: House, cards: Vector[CardDesc]): Vector[CardDesc] = {
    cards.map { c ⇒
      if (c.card.isSpell) c.copy(enabled = false)
      else c
    }
  }
}

case class AttackFactor(fact: Float) extends AttackFunc {
  def apply(attack: Int): Int = math.ceil(attack * fact).toInt
}

case object SkipTurn extends DescMod {
  def apply(house: House, cards: Vector[CardDesc]): Vector[CardDesc] = {
    cards.map(c ⇒ c.copy(enabled = false))
  }
}

trait OwnerDeathEventListener extends HouseEventListener {
  def reactDead(dead: Dead) {
    if (dead.player.id == player.id) {
      player.slots foreach { s ⇒
        if (s.num != dead.num) {
          val card = s.get.card
          if (card.isSpecial) {
            s.get.reaction onDeath dead
          }
        }
      }
    }
  }
  override def init(p: PlayerUpdate) {
    super.init(p)
    p.slots.onDead after { dead ⇒ reactDead(dead) }
  }
}

trait AnyDeathEventListener extends HouseEventListener {
  def reactDead(dead: Dead) {
    player.slots foreach { s ⇒
      if (dead.player.id != player.id || s.num != dead.num) {
        val card = s.get.card
        if (card.isSpecial) {
          s.get.reaction onDeath dead
        }
      }
    }
  }
  override def init(p: PlayerUpdate) {
    super.init(p)
    p.slots.onDead after reactDead
    p.otherPlayer.slots.onDead after reactDead
  }
}

trait OppDeathEventListener extends HouseEventListener {
  def reactDead(dead: Dead) {
    player.slots foreach { s ⇒
      val card = s.get.card
      if (card.isSpecial) {
        s.get.reaction onDeath dead
      }
    }
  }
  override def init(p: PlayerUpdate) {
    super.init(p)
    p.otherPlayer.slots.onDead after reactDead
  }
}

trait DamageAttack {

  // return amount of damage done
  def damageAndGet(num: Int, d: Damage, player: PlayerUpdate) = {
    val otherPlayer = player.otherPlayer
    val slot = otherPlayer.slots(num)
    slot.value match {
      case None ⇒
        val oldl = otherPlayer.value.life
        otherPlayer inflict d
        oldl - otherPlayer.value.life
      case Some(slotState) ⇒
        val oldl = slotState.life
        slot.inflict(d)
        val newl = slot.value.map(_.life) getOrElse 0
        oldl - newl
    }
  }

  def damageCreatureAndGet(num: Int, d: Damage, player: PlayerUpdate) = {
    val otherPlayer = player.otherPlayer
    val slot = otherPlayer.slots(num)
    slot.value match {
      case None ⇒ 0
      case Some(slotState) ⇒
        val oldl = slotState.life
        slot.inflict(d)
        val newl = slot.value.map(_.life) getOrElse 0
        oldl - newl
    }
  }
}

object PlayerTask {
  val currentId = new java.util.concurrent.atomic.AtomicInteger
}
class CountDown(val count: Int, f: Env ⇒ Unit, val id: Int = PlayerTask.currentId.incrementAndGet) extends Function[Env, Unit] {
  def apply(env: Env) {
    val c = count - 1
    if (c == 0) {
      f(env)
      env.player removeEffect (_ == this)
    } else {
      env.player mapEffect { e ⇒
        if (e == this) {
          new CountDown(count - 1, f, id)
        } else e
      }
    }
  }
  override def equals(o: Any) = {
    o match {
      case c: CountDown ⇒ c.id == id
      case _            ⇒ false
    }
  }
  override def hashCode() = id
}

trait ReactionWithData[A <: AnyRef] extends Reaction {
  def updateData(f: A ⇒ A) {
    selected write selected.value.map(x ⇒ x.copy(data = f(x.data.asInstanceOf[A])))
  }
  def getData = selected.get.data.asInstanceOf[A]
}

case class Destroyed(card: Card) extends DescMod {
  def apply(house: House, cards: Vector[CardDesc]): Vector[CardDesc] = {
    if (house.houseIndex != card.houseIndex) cards
    else cards filter { c ⇒ c.card != card }
  }
}

case class Destroyeds(excls: Set[Card]) extends DescMod {
  def apply(house: House, cards: Vector[CardDesc]): Vector[CardDesc] = {
    cards filterNot { c ⇒ excls contains c.card }
  }
}
