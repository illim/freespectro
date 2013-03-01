package priv.sp

import collection._
import scalaz._

sealed trait Card {
  def image: String
  def isSpell: Boolean
  def inputSpec: Option[CardInputSpec]
  def spec: CardSpec
  def isAvailable(house: HouseState) = cost <= house.mana

  var cost = 0
  var id = 0
  var houseIndex = 0
}
case class Creature(
  name: String,
  attack: Option[Int],
  life: Int,
  inputSpec: Option[CardInputSpec] = Some(SelectOwnerSlot),
  spec: CardSpec = CardSpec.creature(),
  multipleTarget : Boolean = false,
  immune : Boolean = false) extends Card {

  def isSpell = false
  def image = name + ".JPG"
}

case class Spell(
  name: String,
  inputSpec: Option[CardInputSpec] = None,
  spec: CardSpec = CardSpec.spell()) extends Card {

  def isSpell = true
  def image = name + ".tga"
}

case class Command(player: PlayerId, card: Card, input: Option[SlotInput])

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

  @inline def inflictCreature(player: PlayerStateLenses, numSlot : Int, amount : Int, isAbility : Boolean = false) : State[GameState, Unit] = {
    player.slots.%== { slots =>
      slots.get(numSlot) match {
        case None => slots
        case Some(slot) =>
          if (slot.card.immune && isAbility){
            slots
          } else {
            if (slot.life > amount) {
              slots + (numSlot -> SlotState.lifeL.mod(_ - amount, slot))
            } else slots - numSlot
          }
      }
    }
  }

  @inline def inflictCreatures(player: PlayerStateLenses, amount : Int, isAbility : Boolean = false) : State[GameState, Unit] = {
    player.slots.%==( damageSlots(amount, isAbility) _)
  }

  def damageSlots(amount: Int, isAbility : Boolean)(slots: PlayerState.SlotsType) = {
    slots.collect {
      case (num, slot) if (isAbility && slot.card.immune) =>
        num -> slot
      case (num, slot) if slot.life > amount =>
        num -> SlotState.lifeL.mod(_ - amount, slot)
    }
  }
}

case class CardSpec(summon: Boolean, effects: Array[Option[CardSpec.Effect]] = CardSpec.noEffects )


