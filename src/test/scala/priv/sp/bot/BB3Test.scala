package priv.sp.bot
/**
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
        get(Earth, 3, 5, 8, 10),
        get(dudeMancer.Dude, 1, 3, 6, 7))),
      PlayerDesc(Vector(
        get(Fire, 2, 4, 5, 11),
        get(Water, 1, 4, 7, 12),
        get(Air, 1, 2, 7, 10),
        get(Earth, 1, 6, 9, 11),
        get(dudeMancer.Dude, 2, 4, 5, 8)))))

  val heuris = new MultiRatioHeuris(opponent, "Junior", useOppPowerRatio = true, useKillValueRatio = true, usePowerRatio = true, logging = true)

  "e11" should "be better than w12 if lower on life" in {
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
       life = 17)) )

    val w12 = GameState(List(
     pstate(0,
       houseStates(7, 3, 7, 2, 2),
       slots = slots( toSlot(1, dudeMancer.Dude,  6)),
       life = 40),
     pstate(1,
       houseStates(4, 1, 10, 12, 2),
       slots = slots( toSlot(1, Water, 12, life = 11)),
       life = 20)) )

    val stats = playerIds.map( _ => PlayerStats())
    heuris.init(start)
    val xe11 = heuris(e11, stats, 2)
    val xe11o = heuris(e11open, stats, 2)
    val xw12 = heuris(w12, stats, 2)
    println(xe11 + ","+xe11o + "," + xw12)
    println(heuris.logs)
    xe11 should be >(xe11o)
    xe11 should be >(xw12)
  }


  "e5" should "be stopped" in {
    val start = GameState(List(
     pstate(0,
       houseStates(5, 3, 4, 0, 2),
       slots = slots(toSlot(1, Earth,  5)),
       life = 60),
     pstate(1,
       houseStates(3, 4, 5, 4, 2),
       slots = slots(),
       life = 60)) )

    val e5free = GameState(List(
     pstate(0,
       houseStates(8, 6, 7, 9, 5),
       slots = slots(toSlot(1, Earth,  5)),
       life = 52),
     pstate(1,
       houseStates(6, 3, 8, 7, 5),
       slots = slots( toSlot(2, Water,  4)),
       life = 58)) )

    val e5dead = GameState(List(
     pstate(0,
       houseStates(10, 6, 7, 6, 5),
       slots = slots(),
       life = 60),
     pstate(1,
       houseStates(4, 3, 8, 7, 5),
       slots = slots(
         toSlot(1, Fire,  2),
         toSlot(2, Water,  4, life = 9)),
       life = 60)) )

    heuris.logs.clear()
    heuris.init(start)
    val stats = List(PlayerStats(), PlayerStats())
    val xe5free = heuris(e5free, stats, 3)
    val manaratio = PlayerStats.hpManaRatio(Earth.cards(4).asCreature)
    val stats2 = List(PlayerStats(), PlayerStats(nbKill = 1, killValue = manaratio))
    println("e5 = " + manaratio)
    val xe5dead = heuris(e5dead, stats2, 3)
    println(xe5free + ","+xe5dead)
    println(heuris.logs)
    xe5free should be <(xe5dead)
  }

  "bot" should "simulate turns" in {
    val bot = new BoundedBot(1, desc, houses)
     val start = GameState(List(
     pstate(0,
       houseStates(4, 3, 4, 0, 2),
       slots = slots(toSlot(1, Earth,  5)),
       life = 60),
     pstate(1,
       houseStates(3, 4, 5, 4, 2),
       slots = slots(toSlot(2, Water,  4)),
       life = 60)))
    var i = 0
    var state = start
    bot.initGameUpdater(start)
    var player = 0
    while(i < 6){
      val (gameState, transition) = bot.simulateCommand(state, player, None)
      state = gameState
      player = transition.playerId
      i += 1
    }
    println(GameState.toString(state))
  }

}
*/
