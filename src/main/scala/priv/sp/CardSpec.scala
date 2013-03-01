package priv.sp

import collection._

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
  multipleTarget : Boolean = false,
  inputSpec: Option[CardInputSpec] = Some(SelectOwnerSlot),
  spec: CardSpec = CardSpec.creature()) extends Card {

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
    def effectAt(phase : Phase) = {
      val filtereds = effects.collect{ case (ph, f) if ph == phase => f}
      if (filtereds.nonEmpty) {
        val effect = { env : GameCardEffect.Env =>
          filtereds.foldLeft(GameState.unit) { (acc, f) =>
            acc.flatMap(_ => f(env))
          }
        }
        Some(effect)
      } else None
    }

    phases.map(effectAt _)
  }
  val noEffects = phases.map(_ => Option.empty[Effect])
}

case class CardSpec(summon: Boolean, effects: Array[Option[CardSpec.Effect]] = CardSpec.noEffects )


