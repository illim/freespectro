package priv.sp.bot

import priv.sp._
import collection.mutable._
import scala.util.Random
import scalaz._
import org.scalatest._
import org.scalatest.matchers._

/**
 * Shitty random training
 * TODO find relevant changes
 */
class BBBotVs extends FlatSpec with ShouldMatchers {
  val houses = HouseSingleton
  import houses._

  "bot" should "win" in {
    var player = playerIds(scala.util.Random.nextInt(2))
    val shuffle = new CardShuffle(houses)
    val List((p1Desc, p1State), (p2Desc, p2State)) = shuffle.get(List(trooper.Trooper, trooper.Trooper), player)

    var state = GameState(List(PlayerState.init(p1State, p1Desc), PlayerState.init(p2State, p2Desc)))
    val desc = GameDesc(Vector(p1Desc, p2Desc))

    val bots = playerIds.map(id => new BoundedBot(id, desc, houses))
    bots.foreach{ bot =>
      val settings = bot.settings
      settings.duration = 2000
      settings.logging = false
    }

    var results = List.empty[(Int, GameState, Settings)]
    var looser = -1
    var oldlooser = looser
    var i = 0
    var seed = System.currentTimeMillis
    while(i< 10 && (oldlooser == -1 || looser == oldlooser)){
      Random.setSeed(seed)
      oldlooser = looser
      val (turns, endState) = runGame(state, player, bots)
      looser = other(endState.checkEnded.get)
      val settings = bots(looser).settings
      i+= 1
      if (oldlooser == -1 || looser == oldlooser){
        val s = settings.copy()
        results = (turns, endState, s) :: results
        println("lost with " + s)
        mutate(settings)
        println("Trying ("+ i + ") with " + settings)
      }
    }
    if (looser == oldlooser) println("Failed")
    println(bots.map(_.settings))
    results.map{ case (t, st, s) =>
      println(st.players.map(_.life) + " in " + t + " with " + s)
    }
  }

  def mutate(s : Settings){
//    simBlock(s)
//    manaPowMut(s)
//    specialManaPowMut(s)
    //slotOccupation(s)
//    rewardDefaultWeight(s)
    //boost(s)
    attack(s)
    //kill(s)
    //power(s)
    //oppPower(s)
  }

  val simBlock   = Mutate({(s, v) => s.simBlockedSlotWeight = v}, _.simBlockedSlotWeight, (0.5f, 0.9f))
  val manaPowMut = Mutate({(s, v) => s.manaPow = v}, _.manaPow, (1f, 2f))
  val specialManaPowMut = Mutate({(s, v) => s.specialManaPow = v}, _.specialManaPow, (1f, 2f))
  val slotOccupation    = Mutate({(s, v) => s.slotOccupationBonus = v}, _.slotOccupationBonus, (0f, 1f))
  val rewardDefaultWeight = Mutate({(s, v) => s.rewardDefaultWeightFactor = v}, _.rewardDefaultWeightFactor, (0.01f, 0.1f))
  val boost  = Mutate({(s, v) => s.boostFactor = v}, _.boostFactor, (1, 3))
  val attack = Mutate({(s, v) => s.attackBonus = v}, _.attackBonus, (1/50f, 1/2f))
  val kill   = Mutate({(s, v) => s.killFactor = v}, _.killFactor, (0.4f, 1.5f))
  val power   = Mutate({(s, v) => s.powerFactor = v}, _.powerFactor, (0.4f, 1.5f))
  val oppPower   = Mutate({(s, v) => s.oppPowerFactor = v}, _.oppPowerFactor, (0.4f, 1.5f))

  case class Mutate(set : (Settings, Float) => Unit, get : Settings => Float, interval : (Float, Float)){
    def apply(s : Settings){
      val x = interval._1 + Random.nextFloat * (interval._2 - interval._1)
      set(s, x)
    }
  }

  def runGame(init : GameState, startingPlayer : PlayerId, bots : List[Bot]) = {
    var state = init
    var player = startingPlayer
    var turns = 0
    while(state.checkEnded.isEmpty) {
      val bot = bots(player)
      val nextCommand = bot.executeAI(state)
      val (gameState, transition) = bot.simulateCommand(state, player, nextCommand)
      println(player + " summoned " + nextCommand)
      state = gameState
      player = transition.playerId
      println(GameState.toString(state))
      turns += 1
    }
    println(state)
    println(state.players.map(_.life))
    (turns, state)
  }

}
