package priv.sp.bot

import priv.sp._
import collection.mutable._
import scalaz._
import org.scalatest._
import org.scalatest.matchers._

class BBBot2Spec extends BotTestHelper {
  import houses._

  val desc =
    GameDesc(Vector(
      PlayerDesc(Vector(
        get(Fire, 3, 6, 8, 12),
        get(Water, 3, 5, 8, 11),
        get(Air, 3, 6, 8, 10),
        get(Earth, 3, 6, 9, 10),
        get(dudeMancer.Dude, 1, 3, 6, 7))),
      PlayerDesc(Vector(
        get(Fire, 1, 4, 5, 11),
        get(Water, 1, 4, 7, 10),
        get(Air, 1, 2, 7, 10),
        get(Earth, 2, 6, 8, 12),
        get(dudeMancer.Dude, 2, 4, 5, 8)))))

  "bot" should "wait for tornado" in {
    val state = GameState(List(
     pstate(0,
       houseStates(11, 3, 7, 2, 2),
       slots = slots(
         (1 -> toSlotState(Water, 3)),
         (3 -> toSlotState(dudeMancer.Dude, 1))),
       life = 40),
     pstate(1,
       houseStates(3, 2, 9, 3, 1),
       slots = slots(
         (1 -> toSlotState(Earth, 8)),
         (2 -> toSlotState(Fire, 12))),
       life = 40)) )

    val bot = new BoundedBot(1, desc, houses)
    val c = bot.executeAI(state).get.card
    println("card played " + c)
    c should not equal(Air.cards(1))
    c should not equal(Air.cards(6))
  }

}
