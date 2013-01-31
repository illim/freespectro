package priv.sp


case class Command(player: PlayerId, card: Card, inputs: List[CardInput])

case class CardInputSpecs(steps : List[CardInputSpec])
sealed trait CardInputSpec

case object SelectOwnerSlot extends CardInputSpec
case object SelectOwnerCreature extends CardInputSpec
case object SelectTargetCreature extends CardInputSpec

sealed trait CardInput
case class OwnerSlot(num : Int) extends CardInput
case class TargetCreature(num : Int) extends CardInput

sealed trait CardSpec

