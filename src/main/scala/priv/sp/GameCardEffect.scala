package priv.sp

import scalaz._

object GameCardEffect {
  class Env(val playerId: PlayerId, val game: Game) {
    var selected = 0
    @inline def player = game.playersLs(playerId)
    @inline def otherPlayer = game.playersLs(other(playerId))
  }

  def getCardEffect(effect: CardSpec.Effect, env: Env): State[GameState, Unit] = {
    import CardSpec._
    import env._

    effect match {
      case Damage(amount) =>
        otherPlayer.life.%==(_ - amount)
      case DamageCreature(amount) =>
        otherPlayer.slots.%== { slots =>
          slots + (selected -> SlotState.lifeL.mod(_ - amount, slots(selected)))
        }
      case DamageCreatures(amount) =>
        otherPlayer.slots.%==(damageSlots(amount) _)
      case MassDamageCreatures(amount) =>
        player.slots.%==(damageSlots(amount) _).flatMap(_ =>
          otherPlayer.slots.%==(damageSlots(amount) _))
      case Custom(f) => f(env)
    }
  }

  def damageSlots(amount: Int)(slots: PlayerState.SlotsType) = {
    slots.map {
      case (num, slot) =>
        num -> SlotState.lifeL.mod(_ - amount, slot)
    }
  }
}