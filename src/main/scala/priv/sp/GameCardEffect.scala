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
  def damageCreatures(amount : Int) : Effect  = { env : Env => env.otherPlayer.slots.%==(SlotState.damageSlots(amount, isAbility = true) _) }
  def damageCreature(amount : Int) : Effect = { env : Env =>
    SlotState.inflictCreature(env.otherPlayer, env.selected, amount, isAbility = true)
  }
  def massDamage(amount : Int) = { env : Env =>
    env.player.slots.%==(SlotState.damageSlots(amount, isAbility = true) _).flatMap(_ =>
      env.otherPlayer.slots.%==(SlotState.damageSlots(amount, isAbility = true) _))
  }

  def heal(amount : Int) = { env : Env => env.player.life.%==(_ + amount) }
  def healCreature(amount : Int) : Effect  = { env : Env =>
    env.player.slots.%=={ slots =>
      slots + (env.selected -> SlotState.addLife(slots(env.selected), amount))
    }
  }
  def healCreatures(amount : Int) : Effect  = { env : Env =>
    env.player.slots.%=={ slots =>
      slots.map { case (num, slot)  =>
        num -> SlotState.addLife(slot, amount)
      }
    }
  }
}
