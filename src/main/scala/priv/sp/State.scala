package priv.sp

import util.Random
import collection._

case class GameState(players: List[PlayerState]){
  val version = GameState.currentVersion.incrementAndGet()
}
case class PlayerState(
  houses: List[HouseState],
  slots: immutable.TreeMap[Int, SlotState] = immutable.TreeMap.empty,
  life: Int = 60)
case class HouseState(house: House, cards: List[Card], mana: Int)
case class SlotState(card: Creature, life: Int, attack: Int, hasRunOnce: Boolean = false)


import scalaz._
object PlayerState {
  val housesL = Lens.lensu[PlayerState, List[HouseState]]((p, h) => p.copy(houses = h), _.houses)
  val slotsL = Lens.lensu[PlayerState, immutable.TreeMap[Int, SlotState]]((p, s) => p.copy(slots = s), _.slots)
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
  val hasRunOnceL = Lens.lensu[SlotState, Boolean]((p, x) => p.copy(hasRunOnce = x), _.hasRunOnce)
  val toggleRunOnce = hasRunOnceL =>= (_ => true)
}
class PlayerStateLenses(val player : Lens[GameState, PlayerState]){
  val houses = player andThen PlayerState.housesL
  val slots  = player andThen PlayerState.slotsL
  val life   = player andThen PlayerState.lifeL
  def slotsToggleRun = slots.%==(_.map{ case (i, slot) => i -> SlotState.toggleRunOnce(slot) })
  def housesIncrMana = houses.%== (_.map(house => HouseState.manaL.mod(_ + 1, house)))
  def replaceCards(newHouses: List[HouseState]) = houses.%== (_.zipWithIndex.map{case (house, i) => house.copy(cards = newHouses(i).cards) })
}
object GameState {
  val playersL = Lens.lensu[GameState, List[PlayerState]]((p, x) => p.copy(players = x), _.players)
  def playerLens(id : Int) = new PlayerStateLenses(Lens.lensu[GameState, PlayerState]((p, x) => p.copy(players = p.players.updated(id, x)), _.players(id)))
  val unit = State[GameState, Unit](gs => (gs, ()))
  val currentVersion = new java.util.concurrent.atomic.AtomicInteger(0)
}
