package priv.sp.bot

import java.nio.file.{Files, Paths}
import priv.sp._
import collection.mutable._
import scalaz._
import org.scalatest._
import org.scalatest.matchers._

class BBBotSpec extends BotTestHelper {
  import houses._

  val desc =
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

  /**"bot" should "win the game with last move" in {
    val state = GameState(List(
     pstate(0,
       houseStates(10, 10, 10, 10, 10),
       slots = slots(
         toSlot(0, Fire, 10, 10),
         toSlot(1, Water, 11),
         toSlot(2, Earth, 10, 10),
         toSlot(3, Earth, 10, 10),
         toSlot(4, Earth, 10, 10)),
       life = 15),
     pstate(1,
       houseStates(10, 7, 10, 10, 10),
       slots = slots(
         toSlot(0, Water, 10, 7),
         toSlot(5, Earth, 7)),
       life = 5) ))

    val bot = new BoundedBot2(1, desc, houses)
    bot.executeAI(state).get.card should equal(Earth.cards(5))
  }*/

  "bot" should "defend" in {
    val state = GameState(List(
     pstate(0,
       houseStates(11, 10, 7, 10, 10),
       slots = slots(
         toSlot(0, Fire, 10, attack = 11),
         toSlot(4, Earth, 10, attack = 10)), // about to kill next turn
       life = 15),
     pstate(1,
       houseStates(10, 2, 10, 10, 10), // would arma the turn after
       slots = slots(
         toSlot(0, Water, 10, attack = 2),
         toSlot(5, Earth, 7)),
       life = 5) ))

    val bot = new BoundedBot2(1, desc, houses)
    val ((node, played), observer) = bot.debugExecuteAI(state)
    Files.write(Paths.get("bot.json"), BoundedBot2AI.dump(node, observer).getBytes)
    println(played.get)
    played.get.card should equal(Air.cards(3))
  }
}
