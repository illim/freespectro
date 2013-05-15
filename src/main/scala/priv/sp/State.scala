package priv.sp

import collection._
import java.io.Serializable

case class GameState(players: List[PlayerState]) {
  def checkEnded = players.zipWithIndex.collectFirst{ case (p, n) if p.life <= 0 => other(n) }
}
case class PlayerState(
  houses     : PlayerState.HousesType,
  descReader : DescReader,
  slots      : PlayerState.SlotsType = PlayerState.emptySlots,
  life       : Int = 60,
  effects    : List[CardSpec.PhaseEffect] = Nil)
class HouseState(val mana: Int) extends AnyVal with Serializable
case class SlotState(card: Creature, life: Int, status : Int, attackSources: AttackSources, attack : Int, data : AnyRef = null){

  def inflict(damage : Damage) : Option[SlotState] = {
    val newlife = card.inflict(damage, life)
    if (newlife < 1) None else Some(copy(life = newlife))
  }

  def has(flag : Int)= (status & flag) != 0
  def isRunnable = ((status - CardSpec.runFlag) & CardSpec.stunFlag) == 0
}

// Description (should not change during the game)
case class GameDesc(players : Array[PlayerDesc])
case class PlayerDesc(houses : Array[PlayerHouseDesc]){
  houses.zipWithIndex.foreach{ case (house, i) => house.index = i}

  def getIndexOfCardInHouse(card : Card) = {
    houses.find(_.cards.contains(card)).map(_.cards.indexOf(card)).getOrElse(-1)
  }
}
case class PlayerHouseDesc(house : House, cards : Array[Card]){
  var index = 0
  val cardList = cards.toList
}

object PlayerState {
  type SlotsType = immutable.TreeMap[Int, SlotState]
  type HousesType = Vector[HouseState]
  val emptySlots = immutable.TreeMap.empty[Int, SlotState]
  def init(houseState : PlayerState.HousesType, desc : PlayerDesc) = PlayerState(houseState, new DescReader(desc), effects = desc.houses(4).house.effects)
}
object SlotState {
  @inline def addLife(slot : SlotState, amount : Int) = {
    slot.copy(life = math.min(slot.card.life, slot.life + amount))
  }
}

import scalaz._
object GameDesc {
  val playersL = Lens.lensu[GameDesc, Array[PlayerDesc]]((p, x) => p.copy(players = x), _.players)
  def playerLens(id : Int) = Lens.lensu[GameDesc, PlayerDesc]((p, x) => p.copy(players = p.players.updated(id, x)), _.players(id))
  val housesL = Lens.lensu[PlayerDesc, Array[PlayerHouseDesc]]((p, h) => p.copy(houses = h), _.houses)
}

// TODO
class DescReader(val playerDesc : PlayerDesc) {

}
