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
  def spec: CardSpec
  def isAvailable(house: HouseState) = cost <= house.mana
  def description : String

  var cost = 0
  var id = Card.currentId.incrementAndGet
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
  name: String,
  attack: Option[Int],
  life: Int,
  description : String = "",
  inputSpec: Option[CardInputSpec] = Some(SelectOwnerSlot),
  spec: CardSpec = CardSpec.creature(),
  mod : Option[Mod] = None,
  boardEffect : Option[BoardEffect] = None,
  multipleTarget : Boolean = false,
  immune : Boolean = false,
  isFocusable : Boolean = true) // card is focused on effect after spawn. a bit ugly should be specified by effects
     extends Card {
  def this() = this(null, None, 0)

  def inflict(damage : Damage, life : Int) = {
    if (damage.isEffect && immune) life else life - damage.amount
  }
  val runOnce = false
  def image = name + ".JPG"
}

case class Spell(
  name: String,
  description : String = "",
  inputSpec: Option[CardInputSpec] = None,
  spec: CardSpec = CardSpec.spell()) extends Card {
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
  val phases = Array(Direct, OnTurn)

  type Effect = GameCardEffect.Env => Unit
  type PhaseEffect = (CardSpec.Phase, CardSpec.Effect)

  def creature(effects: PhaseEffect*) = CardSpec(true, toEffectMap(effects))
  def spell(effects: PhaseEffect*) = CardSpec(false, toEffectMap(effects))

  def toEffectMap(effects : Traversable[PhaseEffect]) ={
    def effectAt(phase : Phase) : Option[Effect] = {
      val filtereds = effects.collect{ case (ph, f) if ph == phase => f}
      if (filtereds.nonEmpty) {
        Some(new ComposedEffect(filtereds))
      } else None
    }

    phases.map(effectAt _)
  }
  val noEffects = phases.map(_ => Option.empty[Effect])

  class ComposedEffect(effects : Traversable[Effect]) extends Function[GameCardEffect.Env, Unit]{
    def apply(env : GameCardEffect.Env) = {
      effects.foreach(_(env))
    }
  }
}

case class CardSpec(
  summon: Boolean,
  effects: Array[Option[CardSpec.Effect]] = CardSpec.noEffects )

// mods are gathered at player state level, and are not dependent on the slots
trait Mod
case class SpellMod(modify : Int => Int) extends Mod
case class SpellProtectOwner(modify : Int => Int) extends Mod

// board effect are applied per slot during board change and affect own slots
trait BoardEffect
case class AddAttack(amount : Int, around : Boolean = false) extends BoardEffect
case class Reborn(player : PlayerState => Boolean) extends BoardEffect
case object ToggleRunAround extends BoardEffect
case class InterceptSpawn(damage : Damage) extends BoardEffect
