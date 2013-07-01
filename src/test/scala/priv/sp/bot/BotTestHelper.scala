package priv.sp.bot

import priv.sp._
import collection.mutable._
import scalaz._
import org.scalatest._
import org.scalatest.matchers._

trait BotTestHelper extends FlatSpec with ShouldMatchers {
  val houses = HouseSingleton
  import houses._

  def desc : GameDesc

  def get(h : House, is : Int*) = PlayerHouseDesc(h, is.to[Vector].map(i => h.cards(i -1)).map(c => CardDesc(c, c.cost, true)))

  def houseStates(ms : Int*) = ms.to[Vector].map(m => new HouseState(m))
  def toSlotState(h : House, i : Int, x: Int = 0) = {
    val card = h.cards(i -1).asCreature
    SlotState(card, card.life, 1, card.attack, card.attack.base.getOrElse(x), card.data)
  }
  def slots(ss : (Int, SlotState)*) = PlayerState.emptySlots ++ ss.toList
  def pstate(id: Int, hs : Vector[HouseState], slots : PlayerState.SlotsType, life : Int) = PlayerState(hs, new DescReader(desc.players(id)), slots, life = life)

}