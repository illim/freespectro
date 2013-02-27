package priv.sp

import scalaz._

object GameCardEffect {
  class Env(val playerId: PlayerId, val game: Game) {
    var selected = 0
    @inline def player = game.playersLs(playerId)
    @inline def otherPlayer = game.playersLs(other(playerId))
  }

  def damage(amount : Int) = { env : Env => env.otherPlayer.life.%==(_ - amount) }
  def damageCreatures(amount : Int) = { env : Env => env.otherPlayer.slots.%==(damageSlots(amount) _) }
  def damageCreature(amount : Int) = { env : Env =>
    env.otherPlayer.slots.%== { slots =>
      if (slots(env.selected).life > amount) {
        slots + (env.selected -> SlotState.lifeL.mod(_ - amount, slots(env.selected)))
      } else slots - env.selected
    }
  }
  def massDamage(amount : Int) = { env : Env =>
    env.player.slots.%==(damageSlots(amount) _).flatMap(_ =>
      env.otherPlayer.slots.%==(damageSlots(amount) _))
  }

  def damageSlots(amount: Int)(slots: PlayerState.SlotsType) = {
    slots.collect {
      case (num, slot) if slot.life > amount =>
        num -> SlotState.lifeL.mod(_ - amount, slot)
    }
  }
}
