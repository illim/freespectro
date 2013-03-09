package priv.sp

import scalaz._
import CardSpec._

object GameCardEffect {

  // state is used directly when it is evaluated late enough
  class Env(val playerId: PlayerId, val game: Game) {
    var selected = 0
    @inline def player = game.playersLs(playerId)
    @inline def otherPlayer = game.playersLs(other(playerId))
    @inline def getMana(houseIndex : Int) : Int = game.state.players(playerId).houses(houseIndex).mana
    @inline def guard(amount : Int) = game.state.players(other(playerId)).guard(amount)
    @inline def guardSelf(amount : Int) = game.state.players(playerId).guard(amount)

    def mod(d : Damage) = {
      if (d.isSpell) {
        d.copy(amount = ((d.amount /: game.state.players(playerId).mods){
          case (acc, m : SpellMod) => m.modify(acc)
          case (acc, _) => acc
        }))
      } else d
    }
  }

  def damage(d : Damage) = { env : Env => env.otherPlayer.life.%==(_ - env.guard(env.mod(d).amount)) }
  def damageCreatures(d : Damage) : Effect  = { env : Env => env.otherPlayer.slots.%==(SlotState.damageSlots(env.mod(d)) _) }
  def damageCreature(d: Damage) : Effect = { env : Env =>
    SlotState.inflictCreature(env.otherPlayer, env.selected, env.mod(d))
  }
  def massDamage(d : Damage) = { env : Env =>
    val damage = env.mod(d)
    env.player.slots.%==(SlotState.damageSlots(damage) _).flatMap(_ =>
      env.otherPlayer.slots.%==(SlotState.damageSlots(damage) _))
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

  def addMana(amount : Int, houseIndex : Int*) = {env : Env =>
    env.player.houses.%=={ houses =>
      HouseState.incrMana(houses, amount, houseIndex : _*)
    }
  }

  def toggleRun = {env : Env =>
    env.player.slots.%=={ slots =>
      slots + (env.selected -> slots(env.selected).copy(hasRunOnce = true))
    }
  }

}

trait HouseCardEffects {
  import GameCardEffect._

  // specific card effects

  def inferno = { env: Env =>
    import env._
    otherPlayer.slots.%== { slots =>
      var result = PlayerState.emptySlots
      val damage = env.mod(Damage(10, isSpell = true))
      slots.foreach { case (num, slot) =>
        slot.inflict(if (num == selected) env.mod(Damage(18, true)) else damage).foreach { newslot =>
          result += (num -> newslot)
        }
      }
      result
    }
  }

  def goblinBerserker = { env: Env =>
    env.player.slots.%== { slots =>
      var result = PlayerState.emptySlots
      val damage = Damage(2, isAbility = true)
      slots.foreach {
        case (num, slot) =>
          if (math.abs(num - env.selected) == 1) {
            slot.inflict(damage).foreach { newslot =>
              result += (num -> newslot)
            }
          } else {
            result += (num -> slot)
          }
      }
      result
    }
  }

  def bargul = { env: Env =>
    import env._
    val damage = Damage(4, isAbility = true)

    otherPlayer.slots.%==(SlotState.damageSlots(damage) _).flatMap{_ =>
      player.slots.%=={ slots =>
        var result = PlayerState.emptySlots
        slots.foreach {
          case (num, slot) =>
            if (num != selected) {
              slot.inflict(damage).foreach { newslot =>
                result += (num -> newslot)
              }
            } else {
              result += (num -> slot)
            }
        }
        result
      }
    }
  }
}
