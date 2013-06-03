package priv.sp.bot

import priv.sp._

trait Heuris {
  def apply(state : GameState) : Float
}

abstract class AIHeuristic[A](botPlayerId : PlayerId) extends Heuris {
  val human = other(botPlayerId)
  var start = null.asInstanceOf[A]
  def name : String
  def init(st : GameState){ start = getValue(st)  }
  def getValue(state : GameState) : A
  def apply(state : GameState) : Float
}

class LifeHeuris(botPlayerId : PlayerId) extends AIHeuristic[Int](botPlayerId){
  val name = "Rushomon"
  def getValue(state : GameState) = state.players(botPlayerId).life - state.players(human).life
  def apply(state : GameState) = getValue(state) - start
}

class LifeManaRatioHeuris(botPlayerId : PlayerId) extends AIHeuristic[(Int, Int)](botPlayerId){
  val name = "Leonard"
  def getMana(p : PlayerState) = p.houses.map(_.mana).sum
  def getValue(state : GameState) = {
    (state.players(botPlayerId).life - state.players(human).life,
     getMana(state.players(botPlayerId)) - getMana(state.players(human)))
  }
  def apply(state : GameState) : Float = {
    val (l, m) = getValue(state)
    val ratio = if (m >= start._2) 1/(1f+ m - start._2) else (1 + (start._2 - m) / 2f)
    (l - start._1) / ratio
  }
}

class BoardLifeManaRatioHeuris(botPlayerId : PlayerId) extends AIHeuristic[(Int, Int, Int)](botPlayerId){
  val name = "Pierrot"
  def getMana(p : PlayerState) = p.houses.map(_.mana).sum
  def getValue(state : GameState) = {
    val botp = state.players(botPlayerId)
    val humanp = state.players(human)
    (botp.life - humanp.life, getMana(botp) - getMana(humanp), botp.slots.values.map(_.attack).toList.sum - humanp.slots.values.map(_.attack).toList.sum) // very stupid board eval
  }
  def apply(state : GameState) : Float = {
    val humanp = state.players(human)
    val (l, m, b) = getValue(state)
    // early game
    if (getMana(humanp) < 30 && m < 10 && humanp.life > 40 && l < 10){
      val mratio = if (m <= start._2) 1/(1f+ start._2 -m) else (1 + (m - start._2)/2f)
      val bratio = if (b <= start._3) 1/(1f+ start._2 -b) else (1 + (b - start._3)/2f)
       mratio * bratio / (1 + (l - start._1) / 2f)
    } else {
      val mratio = if (m >= start._2) 1/(1f+ m - start._2) else (1 + (start._2 - m)/2f)
      val bratio = if (b >= start._3) 1/(1f+ b - start._3) else (1 + (start._3 - b)/2f)
      (l - start._1) / (mratio * bratio)
    }
  }
}
