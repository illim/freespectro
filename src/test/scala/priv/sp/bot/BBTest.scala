package priv.sp.bot

import priv.sp._
import collection.mutable._
import scalaz._
import org.scalatest._
import org.scalatest.matchers._

class BBBotSpec extends FlatSpec with ShouldMatchers {
  val houses = HouseSingleton
  import houses._

  val desc = {
    def get(h : House, is : Int*) = PlayerHouseDesc(h, is.to[Vector].map(i => h.cards(i -1)).map(c => CardDesc(c, c.cost, true)))

    GameDesc(Vector(
      PlayerDesc(Vector(
        get(Fire, 3, 6, 8, 10),
        get(Water, 2, 4, 7, 11),
        get(Air, 3, 6, 8, 10),
        get(Earth, 3, 6, 8, 10),
        get(dudeMancer.Dude, 1, 3, 6, 7))),
      PlayerDesc(Vector(
        get(Fire, 1, 4, 5, 11),
        get(Water, 1, 5, 6, 10),
        get(Air, 1, 4, 8, 12),
        get(Earth, 2, 6, 7, 11),
        get(dudeMancer.Dude, 2, 4, 5, 8)))))
  }

  def houseStates(ms : Int*) = ms.to[Vector].map(m => new HouseState(m))
  def toSlotState(h : House, i : Int, x: Int = 0) = {
    val card = h.cards(i -1).asCreature
    SlotState(card, card.life, card.status, card.attack, card.attack.base.getOrElse(x), card.data)
  }
  def slots(ss : (Int, SlotState)*) = PlayerState.emptySlots ++ ss.toList
  def pstate(id: Int, hs : Vector[HouseState], slots : PlayerState.SlotsType, life : Int) = PlayerState(hs, new DescReader(desc.players(id)), slots, life = life)

  "bot" should "win the game with last move" in {
    val state = GameState(List(
     pstate(0,
       houseStates(10, 10, 10, 10, 10),
       slots = slots(
         (0 -> toSlotState(Fire, 10, 10)),
         (1 -> toSlotState(Water, 11)),
         (2 -> toSlotState(Earth, 10, 10)),
         (3 -> toSlotState(Earth, 10, 10)),
         (4 -> toSlotState(Earth, 10, 10))),
       life = 15),
     pstate(1,
       houseStates(10, 10, 10, 10, 10),
       slots = slots(
         (0 -> toSlotState(Water, 10, 7)),
         (5 -> toSlotState(Earth, 7))),
       life = 5) ))

    val bot = new BoundedBot(1, desc, houses)
    bot.executeAI(state).get.card should equal(Earth.cards(5))
  }

  "bot" should "defend" in {
    val state = GameState(List(
     pstate(0,
       houseStates(11, 10, 7, 10, 10),
       slots = slots(
         (0 -> toSlotState(Fire, 10, 11)),
         (4 -> toSlotState(Earth, 10, 10))),
       life = 15),
     pstate(1,
       houseStates(10, 2, 10, 10, 10),
       slots = slots(
         (0 -> toSlotState(Water, 10, 2)),
         (5 -> toSlotState(Earth, 7))),
       life = 5) ))

    val bot = new BoundedBot(1, desc, houses)
    bot.executeAI(state).get.card should equal(Air.cards(3))
  }

}
