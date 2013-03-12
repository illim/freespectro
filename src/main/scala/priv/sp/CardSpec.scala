package priv.sp

import collection._
import scalaz._

sealed trait Card {
  def name : String
  def image: String
  def inputSpec: Option[CardInputSpec]
  def spec: CardSpec
  def isAvailable(house: HouseState) = cost <= house.mana

  var cost = 0
  var id = 0
  var houseIndex = 0
  final val isSpell = isInstanceOf[Spell]
  override def toString() = s"Card($name)"
  override def hashCode() : Int = cost + houseIndex * 32
  override def equals(o : Any) = {
    o match {
      case c : Card => c.hashCode() == hashCode()
      case _ => false
    }
  }
}
case class Creature(
  name: String,
  attack: Option[Int],
  life: Int,
  inputSpec: Option[CardInputSpec] = Some(SelectOwnerSlot),
  spec: CardSpec = CardSpec.creature(),
  mod : Option[Mod] = None,
  boardEffect : Option[BoardEffect] = None,
  multipleTarget : Boolean = false,
  immune : Boolean = false) extends Card {

  def inflict(damage : Damage, life : Int) = {
    if (damage.isEffect && immune) life else life - damage.amount
  }
  val runOnce = false
  def image = name + ".JPG"
}

case class Spell(
  name: String,
  inputSpec: Option[CardInputSpec] = None,
  spec: CardSpec = CardSpec.spell()) extends Card {

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

class SlotInput(val num: Int) extends AnyVal

object CardSpec {
  type Phase = Int
  val Direct = 0
  val OnTurn = 1
  val phases = Array(Direct, OnTurn)

  type Effect = GameCardEffect.Env => scalaz.State[GameState, Unit]
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

  class ComposedEffect(effects : Traversable[Effect]) extends Function[GameCardEffect.Env, scalaz.State[GameState, Unit]]{
    def apply(env : GameCardEffect.Env) = {
      effects.foldLeft(GameState.unit) { (acc, f) =>
        acc.flatMap(_ => f(env))
      }
    }
  }
}

case class CardSpec(
  summon: Boolean,
  effects: Array[Option[CardSpec.Effect]] = CardSpec.noEffects )

// mods are gathered at player state level, and are not dependent on the slots
trait Mod
class SpellMod(val modify : Int => Int) extends Mod
class SpellProtectOwner(val modify : Int => Int) extends Mod
case class InterceptSpawn(damage : Damage) extends Mod // TODO manage as an effect (anim)

// board effect are applied per slot during board change
trait BoardEffect
case class AddAttack(amount : Int, around : Boolean = false) extends BoardEffect
case class Reborn(player : PlayerState => Boolean) extends BoardEffect
case object ToggleRunAround extends BoardEffect
