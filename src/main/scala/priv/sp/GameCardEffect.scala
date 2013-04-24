package priv.sp

import collection._
import scalaz._
import CardSpec._

object GameCardEffect {

  // state is used directly when it is evaluated late enough
  class Env(val playerId: PlayerId, val updater: GameStateUpdater) {
    var selected = 0

    @inline def player = updater.players(playerId)
    @inline def otherPlayer = updater.players(other(playerId))
    @inline def getMana(houseIndex : Int) : Int = updater.state.players(playerId).houses(houseIndex).mana
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
    env.player.slots.update{ slots =>
      slots + (env.selected -> SlotState.addLife(slots(env.selected), amount))
    }
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

trait HouseCardEffects {
  import GameCardEffect._

  // specific card effects


  def fury = { env: Env =>
    import env._

    val attack = (player.slots.slots.values.map(_.attack)(breakOut) : Seq[Int]).sorted(math.Ordering.Int.reverse).take(2).sum
    env.otherPlayer.inflict(Damage(attack, isSpell = true))
  }


  def spider = { env: Env =>
    def spawnForestSpiderAt(num : Int){
      if (env.player.slots.slots.get(num).isEmpty && num > -1 && num < 6){
        env.player.slots.add(num, SlotState.asCreature(Houses.forestSpider))
      }
    }
    spawnForestSpiderAt(env.selected - 1)
    spawnForestSpiderAt(env.selected + 1)
  }

}
