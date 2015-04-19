package priv.sp.bot

import scala.collection._
import priv.sp._
import priv.sp.update._


// simple life delta
class LifeHeuris(context : BotContext, val settings : Settings) {
  import context._
  val name = "Rushomon" // not a real rusher
  var start = null.asInstanceOf[HeurisValue]

  def init(st : GameState){ start = new HeurisValue(st) }
  def apply(state : GameState) : Float = {
    math.max(0f, (40f + ((new HeurisValue(state)).lifeDelta - start.lifeDelta).toFloat) / 100f)
  }

  class HeurisValue(state : GameState){
    private val bot = state.players(botPlayerId)
    private val human = state.players(humanId)

    val lifeDelta = bot.life - human.life
  }

}
