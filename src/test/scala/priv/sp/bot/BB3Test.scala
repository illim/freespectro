package priv.sp.bot

import priv.sp._
import priv.sp.update._
import collection.mutable._
import scalaz._
import org.scalatest._
import org.scalatest.matchers._

class BBBot3Spec extends BotTestHelper {
  import houses._

  val desc =
    GameDesc(Vector(
      PlayerDesc(Vector(
        get(Fire, 3, 6, 8, 12),
        get(Water, 3, 5, 8, 11),
        get(Air, 3, 6, 8, 10),
        get(Earth, 3, 6, 8, 10),
        get(dudeMancer.Dude, 1, 3, 6, 7))),
      PlayerDesc(Vector(
        get(Fire, 1, 4, 5, 11),
        get(Water, 1, 4, 7, 12),
        get(Air, 1, 2, 7, 10),
        get(Earth, 1, 6, 9, 11),
        get(dudeMancer.Dude, 2, 4, 5, 8)))))

  "heuris" should "can eval board" in {
    val start = GameState(List(
     pstate(0,
       houseStates(7, 3, 7, 2, 2),
       slots = slots(toSlot(1, dudeMancer.Dude,  6)),
       life = 40),
     pstate(1,
       houseStates(3, 12, 9, 11, 1),
       slots = slots(),
       life = 20)) )

    val e11 = GameState(List(
     pstate(0,
       houseStates(8, 4, 8, 3, 3),
       slots = slots(toSlot(1, dudeMancer.Dude,  6)),
       life = 40),
     pstate(1,
       houseStates(4, 13, 10, 1, 2),
       slots = slots( toSlot(1, Earth, 11, life = 30)),
       life = 23)) )

    val e11open = GameState(List(
     pstate(0,
       houseStates(8, 4, 8, 3, 3),
       slots = slots( toSlot(1, dudeMancer.Dude,  6)),
       life = 40),
     pstate(1,
       houseStates(4, 13, 10, 1, 2),
       slots = slots( toSlot(2, Earth, 11)),
       life = 1)) )

    val w12 = GameState(List(
     pstate(0,
       houseStates(7, 3, 7, 2, 2),
       slots = slots( toSlot(1, dudeMancer.Dude,  6)),
       life = 40),
     pstate(1,
       houseStates(4, 1, 10, 12, 2),
       slots = slots( toSlot(1, Water, 12, life = 11)),
       life = 20)) )

    val heuris = new MultiRatioHeuris(opponent, "Junior", useOppPowerRatio = true, useKillValueRatio = true, useBoardRatio = true, usePowerRatio = true)
    val stats = playerIds.map( _ => PlayerStats())

    heuris.init(start)
    val xe11 = heuris(e11, stats, 2)
    val xe11o = heuris(e11open, stats, 2)
    val xw12 = heuris(w12, stats, 2)
    println(xe11 + ","+xe11o + "," + xw12)
    xe11 should be >(xe11o)
    xe11 should be >(xw12)
  }

}
