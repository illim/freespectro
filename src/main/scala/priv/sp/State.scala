package priv.sp

import util.Random
import collection._

case class GameState(players: List[PlayerState])
case class PlayerState(
  houses: List[HouseState],
  slots: PlayerState.SlotsType = immutable.Map.empty,
  life: Int = 60)
case class HouseState(mana: Int) extends AnyVal
case class SlotState(card: Creature, life: Int, attack: Int, hasRunOnce: Boolean = false)

// Description (should not change during the game)
case class GameDesc(players : List[PlayerDesc])
case class PlayerDesc(houses : List[PlayerHouseDesc]){
  houses.zipWithIndex.foreach{ case (house, i) => house.index = i}
}
case class PlayerHouseDesc(house : House, cards : List[Card]){
  var index = 0
}

import scalaz._
object PlayerState {
  type SlotsType = immutable.Map[Int, SlotState]
  val housesL = Lens.lensu[PlayerState, List[HouseState]]((p, h) => p.copy(houses = h), _.houses)
  val slotsL = Lens.lensu[PlayerState, SlotsType]((p, s) => p.copy(slots = s), _.slots)
  val lifeL = Lens.lensu[PlayerState, Int]((p, l) => p.copy(life = l), _.life)
}
object HouseState{
  val manaL = Lens.lensu[HouseState, Int]((p, x) => p.copy(mana = x), _.mana)
}
object SlotState {
  def creature(card: Card) = {
    card match {
      case creature: Creature => SlotState(creature, creature.life, creature.attack getOrElse 0)
      case _ => sys.error(card + " is not a creature")
    }
  }
  val lifeL = Lens.lensu[SlotState, Int]((p, l) => p.copy(life = l), _.life)
}
class PlayerStateLenses(val player : Lens[GameState, PlayerState]){
  val houses = player andThen PlayerState.housesL
  val slots  = player andThen PlayerState.slotsL
  val life   = player andThen PlayerState.lifeL
  def slotsToggleRun = slots.%==(_.map{ case (i, slot) => i -> slot.copy( hasRunOnce = true) })
  def housesIncrMana = houses.%== (_.map(house => house.copy(mana = house.mana + 1)))
}
object GameState {
  val playersL = Lens.lensu[GameState, List[PlayerState]]((p, x) => p.copy(players = x), _.players)
  def playerLens(id : Int) = new PlayerStateLenses(Lens.lensu[GameState, PlayerState]((p, x) => p.copy(players = p.players.updated(id, x)), _.players(id)))
  val unit = State[GameState, Unit](gs => (gs, ()))
  val currentVersion = new java.util.concurrent.atomic.AtomicInteger(0)
}
// ai hacks
object GameDesc {
  val playersL = Lens.lensu[GameDesc, List[PlayerDesc]]((p, x) => p.copy(players = x), _.players)
  def playerLens(id : Int) = Lens.lensu[GameDesc, PlayerDesc]((p, x) => p.copy(players = p.players.updated(id, x)), _.players(id))
  val housesL = Lens.lensu[PlayerDesc, List[PlayerHouseDesc]]((p, h) => p.copy(houses = h), _.houses)
  def replaceCards(newHouses: List[PlayerHouseDesc]) = housesL.%== (_.zipWithIndex.map{case (house, i) => house.copy(cards = newHouses(i).cards) })
}
