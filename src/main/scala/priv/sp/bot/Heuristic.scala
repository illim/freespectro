package priv.sp.bot

import scala.collection._
import priv.sp._
import priv.sp.update._

/***
 * A few stupid heuristics trying to "catch" different play styles
 */
trait Heuris {
  def name : String
  def init(st : GameState)
  def apply(state : GameState, playerStats : List[PlayerStats], turns : Int) : Float
}

trait HeuristicHelper extends Heuris {
  def botPlayerId : PlayerId
  val humanId = other(botPlayerId)
  var start = null.asInstanceOf[HeurisValue]
  var initState = null.asInstanceOf[GameState]

  def init(st : GameState){
    initState = st
    start = new HeurisValue(st)
  }

  def getKill(s : PlayerStats) = s.nbKill
  def getKillValue(s : PlayerStats) = s.nbKill * (math.max(1, s.killValue / 2)).toFloat
  def fixz[N](x : N)(implicit num : Numeric[N]) = if (x==num.zero) 0.1f else num.toFloat(x)
  def temper(x : Float, ratio : Float) = if (x < 0) x / fixz(ratio) else x * ratio

  class HeurisValue(state : GameState){
    val bot = state.players(botPlayerId)
    val human = state.players(humanId)

    // stupid use of lazy
    lazy val lifeDelta = bot.life - human.life
    lazy val manaDelta = getMana(bot) - getMana(human)
    lazy val botMana = getMana(bot)
    lazy val botPowMana = getPowMana(bot)
    lazy val powMana = getPowMana(human)
    lazy val boardDelta = getBoardValue(bot) - getBoardValue(human)

    def getMana(p : PlayerState) = p.houses.map{ _.mana }.sum
    def getPowMana(p : PlayerState) = p.houses.zipWithIndex.map{ case (x, i) =>
      if (i == 4) math.pow(x.mana, 3) else math.pow(x.mana, 2)
    }.sum.toFloat
    def getBoardValue(p : PlayerState) = {
      p.slots.values.map{s => PlayerStats.hpManaRatio(s) }.sum.toFloat
    }
  }
}

// simple life delta
class LifeHeuris(val botPlayerId : PlayerId) extends HeuristicHelper {
  val name = "Rushomon" // not a real rusher
  def apply(state : GameState, playerStats : List[PlayerStats], turns : Int) = (new HeurisValue(state)).lifeDelta - start.lifeDelta
}

// temper rush a bit with the cost of the life delta
class LifeManaRatioHeuris(val botPlayerId : PlayerId) extends HeuristicHelper{
  val name = "Dealer"

  def apply(state : GameState, playerStats : List[PlayerStats], turns : Int) : Float = {
    val h = new HeurisValue(state)
    val ratio = h.botMana / (5 * turns + start.botMana).toFloat
    temper(h.lifeDelta - start.lifeDelta, ratio)
  }
}

// a soup of shitty indicators
class MultiRatioHeuris(
  val botPlayerId : PlayerId,
  val name : String,
  useKillRatio : Boolean = false,
  useKillValueRatio : Boolean = false,
  useManaRatio : Boolean = true,
  useOppManaRatio : Boolean = false,
  useBoardRatio : Boolean = false,
  lifeThreshold : Float = 1f
) extends HeuristicHelper {

  def apply(state : GameState, playerStats : List[PlayerStats], turns : Int) : Float = {
    val h = new HeurisValue(state)
    val humanLifeRatio = lifeThreshold + (start.human.life - h.human.life) / fixz(math.max(h.human.life, start.human.life))
    var res = humanLifeRatio

    if (useKillRatio){
      val botKill = getKill(playerStats(botPlayerId))
      val allKill = botKill + getKill(playerStats(humanId))
      res = temper(res, (if (allKill == 0) 1f else (0.5f + (botKill / allKill))))
    }

    if (useKillValueRatio){
      val botKill = getKillValue(playerStats(botPlayerId))
      val allKill = botKill + getKillValue(playerStats(humanId))
      res = temper(res, (if (allKill == 0) 1f else (0.5f + (botKill / allKill))))
    }

    if (useManaRatio){
      res = temper(res, h.botPowMana / (turns * fixz(start.botPowMana)))
    }

    if (useOppManaRatio){
      res = temper(res, (turns * start.powMana) / fixz(h.powMana))
    }

    if (useBoardRatio){
      res = temper(res, 0.3f + math.max(0, start.boardDelta / fixz(h.boardDelta)))
    }
    res
  }
}
