package priv.sp.update

import collection._
import scalaz._
import priv.sp._
import priv.util.FieldUpdate

class GameStateUpdater(initState : GameState) extends FieldUpdate(None, initState) { self =>
  private val playerFieldUpdates = playerIds.map(id => new PlayerUpdate(id, self))
  var ended = false
  var updateListener : UpdateListener = new DefaultUpdateListener

  def state = value

  def apply(value : GameState) = {
    ended = false
    initNewUpdate(value)
  }

  def lift[R](f : GameStateUpdater => R) : State[GameState, R] = State{ st : GameState =>
    val update = apply(st)
    val res = f(update)
    (update.result, res)
  }

  def focus(num : Int, playerId : PlayerId, blocking : Boolean = true) = updateListener.focus(num, playerId, blocking)

  def players(id : PlayerId) = playerFieldUpdates(id).reinit()

  def result = {
    playerFieldUpdates.foreach(_.flush())
    state.copy(players = playerIds.map{ id =>
      val fp = playerFieldUpdates(id)
      if (fp.isDirty) fp.result else state.players(id)
    })
  }
}


trait UpdateListener {
  def focus(num : Int, playerId : PlayerId, blocking : Boolean = true)
  def move(num : Int, dest : Int, playerId : PlayerId)
  def runSlot(num : Int, playerId : PlayerId)
  def summon(num : Int, slot : SlotState, playerId : PlayerId)
  def refresh(silent : Boolean = false) // this is not great to have some gui code here
  def spellPlayed(c : Command)
}

class DefaultUpdateListener extends UpdateListener {
  def focus(num : Int, playerId : PlayerId, blocking : Boolean){ }
  def move(num : Int, dest : Int, playerId : PlayerId) {}
  def runSlot(num : Int, playerId : PlayerId){}
  def summon(num : Int, slot : SlotState, playerId : PlayerId){}
  def refresh(silent : Boolean){}
  def spellPlayed(c : Command){}
}
