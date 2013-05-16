package priv.sp

import collection._
import scalaz._
import CardSpec._
import priv.sp.update._

object GameCardEffect {

  class Env(val playerId: PlayerId, val updater: GameStateUpdater) {
    var selected = 0
    var source = Option.empty[SlotSource]

    def focus() = updater.focus(selected, playerId)
    @inline def player = updater.players(playerId)
    @inline def otherPlayer = updater.players(other(playerId))
    @inline def getMana(houseIndex : Int) : Int = updater.state.players(playerId).houses(houseIndex).mana
  }

  def focus(f : Env => Unit) = { env : Env =>
    env.updater.focus(env.selected, env.playerId)
    f(env)
  }
  def damage(d : Damage) = { env : Env => env.otherPlayer.inflict(d, env.source) }
  def damageCreatures(d : Damage) : Effect  = { env : Env => env.otherPlayer.slots.inflictCreatures(d) }
  def damageCreature(d: Damage) : Effect = { env : Env =>
    env.otherPlayer.slots(env.selected).inflict(d)
  }
  def massDamage(d : Damage, immuneSelf : Boolean = false) = { env : Env =>
    if (immuneSelf){
      env.player.slots.value.foreach{ case (num, _) =>
        if (num != env.selected) env.player.slots(num).inflict(d)
      }
    } else {
      env.player.slots.inflictCreatures(d)
    }
    env.otherPlayer.slots.inflictCreatures(d)
  }

  def heal(amount : Int) = { env : Env => env.player.heal(amount) }
  def healCreature(amount : Int) : Effect  = { env : Env =>
    env.player.slots(env.selected).heal(amount)
  }
  def healCreatures(amount : Int) : Effect  = { env : Env =>
    env.player.slots.healCreatures(amount)
  }

  def addMana(amount : Int, houseIndex : Int*) = {env : Env =>
    env.player.houses.incrMana(amount, houseIndex : _*)
  }
  def addDescMod(mod : DescMod) = { env : Env => env.player.addDescMod(mod) }
}

case class Ability(card : Card, ability : Card) extends DescMod {
  def apply(house : House, cards : Vector[CardDesc]) : Vector[CardDesc] = {
    if (house.houseIndex == 4) {
      cards.map{ c =>
        if (c.card == card){
          CardDesc(ability)
        } else c
      }
    } else cards
  }
}
