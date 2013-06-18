package priv.sp.update

import collection._
import scalaz._
import priv.sp._
import priv.util.FieldUpdate

// desc is only used to get house specific listener
class GameStateUpdater(initState : GameState, val desc : GameDesc) extends FieldUpdate(None, initState) { self =>
  val playerFieldUpdates = playerIds.map(id => new PlayerUpdate(id, self))
  var ended = false
  var updateListener : UpdateListener = new DefaultUpdateListener
  val houseEventListeners = playerFieldUpdates.map(_.houseEventListener)
  val stats = playerFieldUpdates.map(_.stats)

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

  def resetStats(){ playerFieldUpdates.foreach(_.stats.reset()) }
  def flush() {  playerFieldUpdates.foreach(_.flush()) }

  def result = {
    flush()
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
  def die(num : Int, playerId : PlayerId)
  def refresh(silent : Boolean = false) // this is not great to have some gui code here
  def spellPlayed(c : Command)
}

class DefaultUpdateListener extends UpdateListener {
  def focus(num : Int, playerId : PlayerId, blocking : Boolean){ }
  def move(num : Int, dest : Int, playerId : PlayerId) {}
  def runSlot(num : Int, playerId : PlayerId){}
  def summon(num : Int, slot : SlotState, playerId : PlayerId){}
  def die(num : Int, playerId : PlayerId){}
  def refresh(silent : Boolean){}
  def spellPlayed(c : Command){}
}

// broadcast crap
class HouseEventListener {
  protected var playerField : PlayerUpdate = null
  def player = playerField.reinit()
  def onDeath(dead : Dead) {}
  def protect(num : Int, damage : Damage) = damage
  def onDamaged(card : Creature, amount : Int, slot : SlotUpdate) {}
  def refreshOnOppUpdate() {} // bullcrap, and should not affect opp(looping is not managed)

  private val isResult = (false, None)
  def interceptSubmit(c : Command) : (Boolean, Option[Command]) = isResult
  def setPlayer(p : PlayerUpdate){ playerField = p  }
}

// bs for warp class
class ProxyEventListener(inner : HouseEventListener) extends HouseEventListener {
  override def onDeath(dead : Dead) { inner.onDeath(dead) }
  override def protect(num : Int, damage : Damage) = inner.protect(num, damage)
  override def onDamaged(card : Creature, amount : Int, slot : SlotUpdate) { inner.onDamaged(card, amount, slot) }
  override def refreshOnOppUpdate() { inner.refreshOnOppUpdate() }
  override def interceptSubmit(c : Command) : (Boolean, Option[Command]) = inner.interceptSubmit(c)
  override def setPlayer(p : PlayerUpdate){
    playerField = p
    inner.setPlayer(p)
  }
}
