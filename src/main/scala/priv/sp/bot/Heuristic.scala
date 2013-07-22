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
  val spells = mutable.Map.empty[Int, Int]
  def settings : Settings


  val cardBoardValues = mutable.Map.empty[Card, Double]

  def init(st : GameState){
    initState = st
    start = new HeurisValue(st)
    // BS lower power of card without turn effects (for orc, spider) supposing the value of the card is impacted elsewhere
    st.players(botPlayerId).desc.get.houses.foreach{ h =>
      h.cards.foreach{ cardDesc =>
        val c = cardDesc.card
        val cardValue = if (c.effects(CardSpec.OnTurn).isEmpty && c.effects(CardSpec.OnEndTurn).isEmpty){
          settings.getCostPowMana(math.max(1, c.cost - 2), c.houseIndex)
        } else settings.getCostPowMana(c.cost, c.houseIndex)
        cardBoardValues += (c -> cardValue)
      }
    }
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

    def getMana(p : PlayerState) = p.houses.map{ _.mana }.sum
    def getPower(p : PlayerState) = {
      (p.houses.zipWithIndex.map{ case (x, i) =>
        settings.getCostPowMana(x.mana, i)
      }.sum + (p.slots.values.map{s =>
        val card = s.card // shitty raw stable board presence
        (cardBoardValues.getOrElseUpdate(card, settings.getCostPowMana(card.cost, card.houseIndex))
         * (1 + s.attack * settings.attackBonus)
         * (settings.slotOccupationBonus + s.life.toFloat / (2 * card.life)))
      }.sum)).toFloat
    }
  }
}

// simple life delta
class LifeHeuris(val botPlayerId : PlayerId, val settings : Settings) extends HeuristicHelper {
  val name = "Rushomon" // not a real rusher
  def apply(state : GameState, playerStats : List[PlayerStats], turns : Int) = (new HeurisValue(state)).lifeDelta - start.lifeDelta
}

// temper rush a bit with the cost of the life delta
class LifeManaRatioHeuris(val botPlayerId : PlayerId, val settings : Settings) extends HeuristicHelper{
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
  val settings : Settings = new Settings,
  useKillValueRatio : Boolean = false,
  usePowerRatio : Boolean = true,
  useOppPowerRatio : Boolean = false,
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

    if (useKillValueRatio){
      val botKill = getKillValue(playerStats(botPlayerId))
      val allKill = botKill + getKillValue(playerStats(humanId))
      val killRatio = if (allKill == 0) 1f else (math.pow(0.5f + (botKill / allKill), settings.killFactor).toFloat)
      killValueLog.log(killRatio)
      res = temper(res, killRatio)
    }

    // this is quite wrong to use mana after lot of simulated turns
    if (usePowerRatio){
      val powerRatio = math.pow(h.botPower / (turns * fixz(start.botPower)), settings.powerFactor).toFloat
      powerRatioLog.log(powerRatio)
      res = temper(res, powerRatio)
    }

    if (useOppPowerRatio){
      val oppPowerRatio = math.pow((turns * start.power) / fixz(h.power), settings.oppPowerFactor).toFloat
      oppPowerRatioLog.log(oppPowerRatio)
      res = temper(res, oppPowerRatio)
    }

    res
  }
}
