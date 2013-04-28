package priv.sp

import collection._
import java.io._

object Card {
  val currentId = new java.util.concurrent.atomic.AtomicInteger
}

sealed abstract class Card extends Externalizable {
  def name : String
  def image: String
  def inputSpec: Option[CardInputSpec]
  def effects: Array[Option[CardSpec.Effect]]
  def isAvailable(house: HouseState) = cost <= house.mana
  def description : String

  var cost = 0
  var id = Card.currentId.incrementAndGet
  var houseId = 0
  var houseIndex = 0
  final val isSpell = isInstanceOf[Spell]
  override def toString() = s"Card($name)"
  override def hashCode() : Int = id
  override def equals(o : Any) = {
    o match {
      case c : Card => c.hashCode() == hashCode()
      case _ => false
    }
  }
  def writeExternal(out : ObjectOutput ){  out.writeInt(id) }
  def readExternal(in : ObjectInput  ){  id = in.readInt() }
  protected def readResolve() : Object = HouseSingleton.getCardById(id)  // this is not great(dunno if i can plug somewhere a serializer for this type)
}

case class Creature(
  name : String,
  attack : Option[Int],
  life : Int,
  description : String = "",
  inputSpec   : Option[CardInputSpec] = Some(SelectOwnerSlot),
  effects     : Array[Option[CardSpec.Effect]] = CardSpec.noEffects,
  mod         : Option[Mod] = None,
  slotEffect  : SlotEffect = CardSpec.defaultSlotEffect,
  reaction    : Reaction = CardSpec.defaultReaction,
  data        : AnyRef = null, // initialize slot custom data
  runAttack   : Attack = SingleTargetAttack,
  immune      : Boolean = false,
  runOnce     : Boolean = false) extends Card {

  def this() = this(null, None, 0)

  def inflict(damage : Damage, life : Int) = {
    if (damage.isEffect && immune) life else life - damage.amount
  }
  def image = name + ".JPG"
}

case class Spell(
  name: String,
  description : String = "",
  inputSpec: Option[CardInputSpec] = None,
  effects : Array[Option[CardSpec.Effect]] = CardSpec.noEffects) extends Card {
  def this() = this(null)
  def image = name + ".tga"
}
case class Command(player: PlayerId, card: Card, input: Option[SlotInput])
case class Damage(amount : Int, isAbility : Boolean = false, isSpell : Boolean = false){
  def isEffect = isAbility || isSpell
}

sealed trait CardInputSpec
case object SelectOwnerSlot extends CardInputSpec
case object SelectOwnerCreature extends CardInputSpec
case object SelectTargetCreature extends CardInputSpec

class SlotInput(val num: Int) extends AnyVal with Serializable

object CardSpec {
  type Phase = Int
  val Direct = 0
  val OnTurn = 1
  val OnEndTurn = 2
  val phases = Array(Direct, OnTurn, OnEndTurn)

  type Effect = GameCardEffect.Env => Unit
  type PhaseEffect = (CardSpec.Phase, CardSpec.Effect)

  def effects(effects: PhaseEffect*) = toEffectMap(effects)

  def toEffectMap(effects : Traversable[PhaseEffect]) ={
    def effectAt(phase : Phase) : Option[Effect] = {
      val filtereds = effects.collect{ case (ph, f) if ph == phase => f}
      if (filtereds.isEmpty) None
      else if (filtereds.size == 1) Some(filtereds.head)
      else Some(new ComposedEffect(filtereds))
    }

    phases.map(effectAt _)
  }
  val noEffects = phases.map(_ => Option.empty[Effect])

  class ComposedEffect(effects : Traversable[Effect]) extends Function[GameCardEffect.Env, Unit]{
    def apply(env : GameCardEffect.Env) = {
      effects.foreach(_(env))
    }
  }
  val defaultSlotEffect = new DefaultSlotEffect
  val defaultReaction = new DefaultReaction
}

trait Mod
case class SpellMod(modify : Int => Int) extends Mod
case class SpellProtectOwner(modify : Int => Int) extends Mod

trait SlotEffect {
  // update elemental or a creature just summoned
  def applySlot(selected : Int, num : Int, slot : SlotState) : SlotState
  // applied when effective creature is summoned
  def applySlots(selected : Int, slots : PlayerState.SlotsType) : PlayerState.SlotsType
  // applied when effective creature is dead
  def unapplySlots(selected : Int, slots : PlayerState.SlotsType) : PlayerState.SlotsType
}

class DefaultSlotEffect extends SlotEffect {
  def applySlot(selected : Int, num : Int, slot : SlotState) : SlotState = slot
  def applySlots(selected : Int, slots : PlayerState.SlotsType) : PlayerState.SlotsType = slots
  def unapplySlots(selected : Int, slots : PlayerState.SlotsType) : PlayerState.SlotsType = slots
}

sealed trait BoardEvent
case class Dead(num : Int, card : Creature, playerId : PlayerId, updater : GameStateUpdater) extends BoardEvent
case class DamageEvent(amount : Int, target : Option[Int], playerId : PlayerId, updater : GameStateUpdater) extends BoardEvent
case class SummonEvent(num : Int, card : Creature, playerId : PlayerId, updater : GameStateUpdater) extends BoardEvent

trait Reaction {
  def onProtect(selected : Int, d : DamageEvent) : Int
  def onDeath(selected : Int, dead : Dead)
  def onSummon(selected : Int, selectedPlayerId : PlayerId, summoned : SummonEvent)
}

class DefaultReaction extends Reaction {
  def onProtect(selected : Int, d : DamageEvent) = d.amount
  def onDeath(selected : Int, dead : Dead) {}
  def onSummon(selected : Int, selectedPlayerId : PlayerId, summoned : SummonEvent) {}
}



trait Attack {
  def apply(num : Int, d : Damage,updater : GameStateUpdater, playerId : PlayerId)
}
object SingleTargetAttack extends Attack {
  def apply(num : Int, d : Damage, updater : GameStateUpdater, id : PlayerId) {
    val otherPlayer = updater.players(other(id))
    otherPlayer.getSlots.get(num) match {
      case None => otherPlayer.inflict(d)
      case Some(oppositeSlot) => otherPlayer.slots.inflictCreature(num, d)
    }
  }
}
object MultiTargetAttack extends Attack {
  def apply(num : Int, d : Damage, updater : GameStateUpdater, id : PlayerId) {
    val otherPlayer = updater.players(other(id))
    otherPlayer.inflict(d)
    otherPlayer.slots.inflictCreatures(d)
  }
}
