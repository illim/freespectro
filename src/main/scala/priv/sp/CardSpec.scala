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
case object Noop extends CardSpec
case object Summon extends CardSpec


case class CardEffects(effects : List[CardEffect])
sealed trait CardEffect
case class Summoned(card : CardState, numSlot : Int) extends CardEffect