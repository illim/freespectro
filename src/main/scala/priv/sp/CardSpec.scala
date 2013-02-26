package priv.sp

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
  sealed trait Phase
  case object Direct extends Phase
  case object OnTurn extends Phase
  case object OnRun extends Phase

  type Effect = GameCardEffect.Env => scalaz.State[GameState, Unit]
  type PhaseEffect = (CardSpec.Phase, CardSpec.Effect)

  def creature(effects: PhaseEffect*) = CardSpec(true, effects.to[List])
  def spell(effects: PhaseEffect*) = CardSpec(false, effects.to[List])
}
case class CardSpec(summon: Boolean, effects: List[CardSpec.PhaseEffect])


