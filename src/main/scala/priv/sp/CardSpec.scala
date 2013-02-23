package priv.sp

sealed trait Card {
  def image: String
  def isSpell: Boolean
  def inputSpec: Option[CardInputSpec]
  def spec: CardSpec
  def isAvailable(house : HouseState) = cost <= house.mana
  var cost = 0
  var id = 0
}
case class Creature(
  name: String,
  attack: Option[Int],
  life: Int,
  inputSpec: Option[CardInputSpec] = Some(SelectOwnerSlot),
  spec: CardSpec = Summon) extends Card {

  def isSpell = false
  def image = name + ".JPG"
}

case class Spell(
  name: String,
  inputSpec: Option[CardInputSpec] = None,
  spec: CardSpec = Noop) extends Card {

  def isSpell = true
  def image = name + ".tga"
}


case class Command(player: PlayerId, card: Card, inputs: List[CardInput])

sealed trait CardInputSpec
case object SelectOwnerSlot extends CardInputSpec
case object SelectOwnerCreature extends CardInputSpec
case object SelectTargetCreature extends CardInputSpec

sealed trait CardInput
case class OwnerSlot(num : Int) extends CardInput
case class TargetCreature(num : Int) extends CardInput

sealed trait CardSpec
case object Noop extends CardSpec
case object Summon extends CardSpec
