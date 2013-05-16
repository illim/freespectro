package priv.sp

import collection._
import java.io._
import priv.sp.update._

object Card {
  val currentId = new java.util.concurrent.atomic.AtomicInteger
}

sealed abstract class Card extends Externalizable {
  def name : String
  def image: String
  def inputSpec: Option[CardInputSpec]
  def effects: Array[Option[CardSpec.Effect]]
  def description : String

  var cost = 0
  var id = Card.currentId.incrementAndGet
  var houseId = 0
  var houseIndex = 0
  final val isSpell = isInstanceOf[Spell]
  def asCreature = {
    this match {
      case creature: Creature => creature
      case _ => sys.error(this + " is not a creature")
    }
  }
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
  attack : AttackSources,
  life : Int,
  description : String = "",
  inputSpec   : Option[CardInputSpec] = Some(SelectOwnerSlot),
  effects     : Array[Option[CardSpec.Effect]] = CardSpec.noEffects,
  mod         : Option[Mod] = None,
  reaction    : Reaction = CardSpec.defaultReaction,
  data        : AnyRef = null, // initialize slot custom data
  runAttack   : RunAttack = SingleTargetAttack,
  immune      : Boolean = false,
  status      : Int = 0) extends Card {

  def this() = this(null, AttackSources(), 0)

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
trait CommandFlag
case class Command(player: PlayerId, card: Card, input: Option[SlotInput], cost : Int, flag : Option[CommandFlag] = None)
case class Damage(amount : Int, isAbility : Boolean = false, isSpell : Boolean = false){
  def isEffect = isAbility || isSpell
}

sealed trait CardInputSpec
case object SelectOwnerSlot extends CardInputSpec
case object SelectOwnerCreature extends CardInputSpec
case object SelectTargetCreature extends CardInputSpec

class SlotInput(val num: Int) extends AnyVal with Serializable

object CardSpec {
  val runFlag = 1
  val stunFlag = 2

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
  val defaultReaction = new DefaultReaction
}

trait Mod
case class SpellMod(modify : Int => Int) extends Mod
case class SpellProtectOwner(modify : Int => Int) extends Mod

sealed trait BoardEvent
case class Dead(num : Int, card : Creature, playerId : PlayerId, updater : GameStateUpdater) extends BoardEvent
// need source if no target
case class DamageEvent(damage : Damage, target : Option[Int], playerId : PlayerId, updater : GameStateUpdater, source : Option[SlotSource]) extends BoardEvent
case class SummonEvent(num : Int, card : Creature, playerId : PlayerId, updater : GameStateUpdater) extends BoardEvent

trait Reaction {
  def onAdd(selected : Int, slot : SlotUpdate)
  def onRemove(slot : SlotUpdate)
  def onProtect(selected : Int, d : DamageEvent) : Damage
  def onMyDeath(dead : Dead)
  def onDeath(selected : Int, dead : Dead)
  def onSummon(selected : Int, selectedPlayerId : PlayerId, summoned : SummonEvent)
  def interceptSubmit(command : Command, updater : GameStateUpdater) : (Boolean, Option[Command])
}

class DefaultReaction extends Reaction {
  def onAdd(selected : Int, slot : SlotUpdate){}
  def onRemove(slot : SlotUpdate){}
  def onProtect(selected : Int, d : DamageEvent) = d.damage
  def onMyDeath(dead : Dead) {}
  def onDeath(selected : Int, dead : Dead) {}
  def onSummon(selected : Int, selectedPlayerId : PlayerId, summoned : SummonEvent) {}
  def interceptSubmit(command : Command, updater : GameStateUpdater) : (Boolean, Option[Command]) = (false, None)
}

case class SlotSource(playerId : PlayerId, num : Int)

trait RunAttack {
  def apply(num : Int, d : Damage,updater : GameStateUpdater, playerId : PlayerId)
}
object SingleTargetAttack extends RunAttack {
  def apply(num : Int, d : Damage, updater : GameStateUpdater, id : PlayerId) {
    val otherPlayer = updater.players(other(id))
    val slot = otherPlayer.slots(num)
    if (slot.value.isEmpty) {
      otherPlayer.inflict(d, Some(SlotSource(id, num)))
    } else {
      slot.inflict(d)
    }
  }
}
object MultiTargetAttack extends RunAttack {
  def apply(num : Int, d : Damage, updater : GameStateUpdater, id : PlayerId) {
    val otherPlayer = updater.players(other(id))
    otherPlayer.inflict(d, Some(SlotSource(id, num)))
    otherPlayer.slots.inflictCreatures(d)
  }
}

object Attack {
  def apply(base : Int) : AttackSources = AttackSources(Some(base))
}

case class AttackSources(base : Option[Int] = None, sources : Vector[AttackSource] = Vector.empty) {
  def add(source : AttackSource)    = copy(sources = sources :+ source)
  def remove(source : AttackSource) = copy(sources = sources.filterNot(_ == source))
}

trait AttackSource
trait AttackFunc extends AttackSource {
  def apply(attack : Int) : Int
}
trait AttackStateFunc extends AttackSource {
  def apply(attack : Int, player : PlayerUpdate) : Int
}
case class ManaAttack(houseIndex : Int) extends AttackStateFunc {
  def apply(attack : Int, player : PlayerUpdate) : Int = attack + player.getHouses(houseIndex).mana
}

trait DescMod {
  def apply(house : House, cards : Vector[CardDesc]) : Vector[CardDesc]
}
