package priv.sp

import collection._
import scalaz._
import CardSpec._

object GameCardEffect {

  // state is used directly when it is evaluated late enough
  class Env(val playerId: PlayerId, val state: GameState) {
    var selected = 0
    @inline def player = playersLs(playerId)
    @inline def otherPlayer = playersLs(other(playerId))
    @inline def getMana(houseIndex : Int) : Int = state.players(playerId).houses(houseIndex).mana
    @inline def guard(amount : Int) = state.players(other(playerId)).guard(amount)
    @inline def guardSelf(amount : Int) = state.players(playerId).guard(amount)

    def mod(d : Damage) = {
      if (d.isSpell) {
        d.copy(amount = ((d.amount /: state.players(playerId).mods){
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

  def fury = { env: Env =>
    import env._

    player.slots.flatMap{ slots =>
      val attack = (slots.values.map(_.attack)(breakOut) : Seq[Int]).sorted(math.Ordering.Int.reverse).take(2).sum
      val d = env.mod(Damage(attack, isSpell = true))
      env.otherPlayer.life.%==(_ - env.guard(env.mod(d).amount))
    }
  }

  def armageddon = { env: Env =>
    import env._

    val d = env.mod(Damage(getMana(0) + 8, isSpell = true))
    env.otherPlayer.life.%==(_ - env.guard(env.mod(d).amount)).flatMap{ _ =>
      massDamage(d)(env)
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

  def spider = {
    env: Env =>
      env.player.slots.%== { slots =>
        var result = slots
        def spawnForestSpiderAt(num : Int){
          if (slots.get(num).isEmpty && num > -1 && num < 6){
            result += (num -> SlotState.creature(Houses.forestSpider))
          }
        }
        spawnForestSpiderAt(env.selected - 1)
        spawnForestSpiderAt(env.selected + 1)
        result
      }
  }


  def cannon = { env: Env =>
    import env._

    otherPlayer.slots.%=={ slots =>
      slots.toSeq.sortBy(_._2.life)(math.Ordering.Int.reverse).headOption match {
        case None => slots
        case Some((num, slot)) =>
          slot.inflict(Damage(8, isAbility = true)) match {
            case None => slots - num
            case Some(slot) => slots + (num -> slot)
          }
      }
    }
  }
}
