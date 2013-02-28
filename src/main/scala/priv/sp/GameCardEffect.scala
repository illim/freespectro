package priv.sp

import scalaz._
import CardSpec._

object GameCardEffect {
  class Env(val playerId: PlayerId, val game: Game) {
    var selected = 0
    @inline def player = game.playersLs(playerId)
    @inline def otherPlayer = game.playersLs(other(playerId))
  }

  def damage(amount : Int) = { env : Env => env.otherPlayer.life.%==(_ - amount) }
  def damageCreatures(amount : Int) : Effect  = { env : Env => env.otherPlayer.slots.%==(damageSlots(amount) _) }
  def damageCreature(amount : Int) : Effect = { env : Env =>
    damageCreature(env.otherPlayer, env.selected, amount)
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

  @inline def damageCreature(player: PlayerStateLenses, numSlot : Int, amount : Int) : State[GameState, Unit] = {
    player.slots.%== { slots =>
      slots.get(numSlot) match {
        case None => slots
        case Some(slot) =>
          if (slot.life > amount) {
            slots + (numSlot -> SlotState.lifeL.mod(_ - amount, slot))
          } else slots - numSlot
      }
    }
  }

  @inline def damageCreatures(player: PlayerStateLenses, amount : Int) : State[GameState, Unit] = {
    player.slots.%==( damageSlots(amount) _)
  }
}
