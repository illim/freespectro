package priv.sp

import collection._
import scalaz._
import CardSpec._

object GameCardEffect {

  // state is used directly when it is evaluated late enough
  class Env(val playerId: PlayerId, val updater: GameStateUpdater) {
    var selected = 0

    def focus() = updater.focus(selected, playerId)
    @inline def player = updater.players(playerId)
    @inline def otherPlayer = updater.players(other(playerId))
    @inline def getMana(houseIndex : Int) : Int = updater.state.players(playerId).houses(houseIndex).mana
  }

  def focus(f : Env => Unit) = { env : Env =>
    env.updater.focus(env.selected, env.playerId)
    f(env)
  }
  def damage(d : Damage) = { env : Env => env.otherPlayer.inflict(d) }
  def damageCreatures(d : Damage) : Effect  = { env : Env => env.otherPlayer.slots.inflictCreatures(d) }
  def damageCreature(d: Damage) : Effect = { env : Env =>
    env.otherPlayer.slots.inflictCreature(env.selected, d)
  }
  def massDamage(d : Damage) = { env : Env =>
    env.player.slots.inflictCreatures(d)
    env.otherPlayer.slots.inflictCreatures(d)
  }

  def heal(amount : Int) = { env : Env => env.player.heal(amount) }
  def healCreature(amount : Int) : Effect  = { env : Env =>
    env.player.slots.healCreature(env.selected, amount)
  }
  def healCreatures(amount : Int) : Effect  = { env : Env =>
    env.player.slots.update{ slots =>
      slots.map { case (num, slot)  =>
        num -> SlotState.addLife(slot, amount)
      }
    }
  }

  def addMana(amount : Int, houseIndex : Int*) = {env : Env =>
    env.player.houses.incrMana(amount, houseIndex : _*)
  }
}
