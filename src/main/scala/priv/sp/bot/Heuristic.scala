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
  def getKillValue(s : PlayerStats) = s.killValue.toFloat
  def fixz[N](x : N)(implicit num : Numeric[N]) = if (x==num.zero) 0.1f else num.toFloat(x)
  def temper(x : Float, ratio : Float) = if (x < 0) x / fixz(ratio) else x * ratio

  class HeurisValue(state : GameState){
    val bot = state.players(botPlayerId)
    val human = state.players(humanId)

    // stupid use of lazy
    lazy val lifeDelta = bot.life - human.life
    lazy val manaDelta = getMana(bot) - getMana(human)
    lazy val botMana = getMana(bot)
    lazy val botPower = getPower(bot)
    lazy val power = getPower(human)
    lazy val boardDelta = getBoardValue(bot) - getBoardValue(human)

    def getMana(p : PlayerState) = p.houses.map{ _.mana }.sum
    def getPower(p : PlayerState) = {
      (p.houses.zipWithIndex.map{ case (x, i) =>
        PlayerStats.getCostPowMana(x.mana, i)
      }.sum + (p.slots.values.map{s =>
        val card = s.card // shitty raw stable board presence
        (PlayerStats.getCostPowMana(card.cost, card.houseIndex) * (1 + s.attack / 20f) * (0.5 + s.life.toFloat / (2 * card.life)))
      }.sum)).toFloat
    }
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
// ratio has to be between 0, +oo so their multiplication is not sign dependent
class MultiRatioHeuris(
  val botPlayerId : PlayerId,
  val name : String,
  useKillRatio : Boolean = false,
  useKillValueRatio : Boolean = false,
  usePowerRatio : Boolean = true,
  useOppPowerRatio : Boolean = false,
  useBoardRatio : Boolean = false,
  lifeThreshold : Float = 0.5f,
  logging : Boolean = false
) extends HeuristicHelper {

  private def maxAbs(x : Int, y : Int) : Float = fixz(math.max(math.abs(x), math.abs(y)))
  def negToPos(x : Float) = if (x < 0) 1 /(1 - x) else 1 + (x / 10f) // divide to have a weight not to high for positive values

  val logs = mutable.Map.empty[Symbol, List[Float]].withDefault(_ => Nil)
  case class ValueLogger(name : Symbol){
    val log =
      if (logging) {x : Float =>  logs += (name -> (x :: logs(name))) }
      else { x: Float => () }
  }

  val lifeRatioLog     = ValueLogger('lifeRatio)
  val powerRatioLog    = ValueLogger('powerRatio)
  val oppPowerRatioLog = ValueLogger('oppPowerRatio)
  val boardRatioLog    = ValueLogger('boardRatio)
  val killValueLog     = ValueLogger('killValue)
  val somehumanId = Some(humanId)

  def apply(state : GameState, playerStats : List[PlayerStats], turns : Int) : Float = {
    val h = new HeurisValue(state)
    val lifeRatio = ((state.checkEnded match {
      case `somehumanId` =>
        negToPos((h.lifeDelta - start.lifeDelta) / maxAbs(h.bot.life, start.bot.life))
      case None => negToPos(h.lifeDelta / h.bot.life.toFloat)
      case _ => negToPos((start.human.life - h.human.life) / maxAbs(h.human.life, start.human.life))
    }) +1 ) / turns
    var res = lifeRatio
    lifeRatioLog.log(lifeRatio)

    if (useKillRatio){
      val botKill = getKill(playerStats(botPlayerId))
      val allKill = botKill + getKill(playerStats(humanId))
      res = temper(res, (if (allKill == 0) 1f else (0.5f + (botKill / allKill))))
    }

    if (useKillValueRatio){
      val botKill = getKillValue(playerStats(botPlayerId))
      val allKill = botKill + getKillValue(playerStats(humanId))
      val killRatio = if (allKill == 0) 1f else (0.5f + (botKill / allKill))
      killValueLog.log(killRatio)
      res = temper(res, killRatio)
    }

    // this is quite wrong to use mana after lot of simulated turns
    if (usePowerRatio){
      val powerRatio = h.botPower / (turns * fixz(start.botPower))
      powerRatioLog.log(powerRatio)
      res = temper(res, powerRatio)
    }

    if (useOppPowerRatio){
      val oppPowerRatio = (turns * start.power) / fixz(h.power)
      oppPowerRatioLog.log(oppPowerRatio)
      res = temper(res, oppPowerRatio)
    }

    if (useBoardRatio){
      val boardRatio = negToPos(h.boardDelta / maxAbs(start.boardDelta.toInt, h.boardDelta.toInt))
      boardRatioLog.log(boardRatio)
      res = temper(res, boardRatio)
    }
    res
  }
}
