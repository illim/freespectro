package priv.sp.house

import priv.sp._
import priv.sp.update._
import priv.sp.gui._
import GameCardEffect._
import CardSpec._

class TestMage {

  val Test : House = House("Test", List(
    Creature("Blargl", Attack(1), 12, "choose another card same turn", effects = effects(Direct -> playTwice)),
    Spell("Focus"),
    Creature("Blargl", Attack(2), 12, effects = effects(Direct -> forceSkip)),
    Creature("Blargl", Attack(3), 12, isAltar = true, reaction = new AltarReaction),
    Creature("Blargl", Attack(4), 12),
    Creature("Blargl", Attack(5), 12),
    Creature("Blargl", Attack(6), 12),
    Creature("Blargl", Attack(7), 12)))

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

  class AltarReaction extends DefaultReaction {
    final override def onOverwrite(c : Creature, slot : SlotUpdate) {
      slot.slots.player.houses.incrMana(c.cost * 2, c.houseIndex)
    }
  }
}
