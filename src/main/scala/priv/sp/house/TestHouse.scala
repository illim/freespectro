package priv.sp.house

import priv.sp._
import priv.sp.update._
import priv.sp.gui._
import GameCardEffect._
import CardSpec._

class TestMage {

  val Test : House = House("Test", List(
    Creature("Blargl", Attack(3), 12, "choose another card same turn", effects = effects(Direct -> playTwice)),
    Spell("Focus"),
    Creature("Blargl", Attack(3), 12, effects = effects(Direct -> forceSkip)),
    Creature("Blargl", Attack(3), 12),
    Creature("Blargl", Attack(3), 12),
    Creature("Blargl", Attack(3), 12),
    Creature("Blargl", Attack(3), 12),
    Creature("Blargl", Attack(3), 12)))

  Test.initCards(Houses.basicCostFunc)

  def playTwice : Effect = { env : Env =>
    env.player.addTransition(WaitPlayer(env.playerId))
  }

  def forceSkip : Effect = { env : Env =>
    env.otherPlayer.addDescMod(SkipTurn)
    env.otherPlayer.addEffect(OnEndTurn -> { e =>
      e.player.removeDescMod(SkipTurn)
    })
  }
}
