package priv.sp.house

import priv.sp._
import priv.sp.update._

/**
 * todo archphoenix
 */
class Elementalist {
  import CardSpec._
  import GameCardEffect._

  val Elementalist = House("Elementalist", List(
    Creature("Sylph", Attack(5), 15, "When enters the game, allows to play additional special card.\n+1 cost for each sylph on the board.", reaction = new SylphReaction, effects = effects(Direct -> sylphEffect)),
    Spell("DeepFreeze", "Both players skip 1 turn and cannot use special cards in their next turn.", effects = effects(Direct -> freeze)),
    Creature("Salamander", Attack(5), 16, "If owner fire power is higher than opponent fire power,\ndeals to opponent 5 damage at the beginning of the turn.", effects = effects(OnTurn -> salamand)),
    Spell("Avalanche", "Deals 2X damage to enemy creatures (X = owner earth power),\nheals 2X life to owner and reduces owner earth power to 0.", effects = effects(Direct -> aval)),
    Spell("Incineration", "Destroys strongest enemy and weakest friendly creatures\n(calculated by health) both on board and in deck.", effects = effects(Direct -> incinerate)),
    Creature("ArchPhoenix", Attack(5), 16, "Fire cards heal him instead of dealing damage."),
    Creature("StoneGolem", Attack(7), 30, "Regenerates 4 life when blocked.\nReceives no damage from spells and creatures abilities when unblocked.", effects = effects(OnTurn -> stoneGole)),
    Spell("FrostLightning", "Deals X damage to opponent\n(X = difference between his lowest power and owner highest power)\nand permanently blocks target slot.",
          inputSpec = Some(SelectTargetCreature),
          effects = effects(Direct -> frostLight))))

  val sylph = Elementalist.cards(0)
  Elementalist.initCards(Houses.basicCostFunc)

  def sylphEffect = { env : Env =>
    env.player.addDescMod(IncrSylphCostMod)
    env.player.addTransition(WaitPlayer(env.playerId))
  }

  def freeze = { env : Env =>
    env.otherPlayer.addDescMod(SkipTurn)
    env.otherPlayer.addEffect(OnEndTurn -> new Unfreeze(true))
  }

  // horror!
  class Unfreeze(chain : Boolean) extends Function[Env, Unit] {
    def apply(env : Env){
      import env._
      player.removeDescMod(SkipTurn)
      player.removeEffect(_.isInstanceOf[Unfreeze])
      if (chain){
        otherPlayer.addDescMod(SkipTurn)
        otherPlayer.addEffect(OnEndTurn -> new Unfreeze(false))
      }
    }
  }

  def salamand = { env : Env =>
    import env._
    if (player.getHouses(0).mana > otherPlayer.getHouses(0).mana){
      focus()
      otherPlayer.inflict(Damage(5, isAbility = true))
    }
  }

  def aval = { env : Env =>
    import env._
    val x = player.getHouses(3).mana
    otherPlayer.slots.inflictCreatures(Damage(2 * x, isSpell = true), env.playerId)
    player.heal(2 * x)
    player.houses.incrMana(-x, 3)
  }

  def incinerate = {env : Env =>
    import env._
    def destroy(s : SlotUpdate){
      val card = s.get.card
      s.destroy()
      s.slots.player.addDescMod(Destroyed(card))
    }
    player.slots.reduce(lowestLife).foreach(destroy)
    otherPlayer.slots.reduce(highestLife).foreach(destroy)
  }

  def stoneGole = { env : Env =>
    import env._
    if (otherPlayer.getSlots.isDefinedAt(selected)){
      player.slots(selected).heal(4)
    }
  }

  def frostLight = { env : Env =>
    import env._
    val opp = otherPlayer.getHouses.reduceLeft((h1, h2) => if (h1.mana < h2.mana) h1 else h2 ).mana
    val own = otherPlayer.getHouses.reduceLeft((h1, h2) => if (h1.mana > h2.mana) h1 else h2 ).mana
    val x = math.max(0, own - opp)
    otherPlayer.inflict(Damage(x, isSpell = true))
    val slot = otherPlayer.slots(env.selected)
    slot.toggle(stunFlag)
  }

  class SylphReaction extends Reaction {
    final override def onMyDeath(dead : Dead) {
      dead.player.removeDescMod(IncrSylphCostMod)
    }
  }

  class SGReaction extends Reaction {
    override def selfProtect(d : Damage, slot : SlotUpdate) = {
      if (slot.slots.player.otherPlayer.getSlots.isDefinedAt(slot.num)){
        if (d.isEffect){
          d.copy(amount = 0)
        } else d
      } else d
    }
  }

  case object IncrSylphCostMod extends DescMod {
    def apply(house : House, cards : Vector[CardDesc]) : Vector[CardDesc] = {
      if (house.houseIndex < 4) cards
      else cards.map{ c =>
        if (c.card == sylph){
          c.copy( cost = c.cost + 1)
        } else c
      }
    }
  }

  case class Destroyed(card : Creature) extends DescMod {
    def apply(house : House, cards : Vector[CardDesc]) : Vector[CardDesc] = {
      if (house.houseIndex != card.houseIndex) cards
      else cards.map{ c =>
        if (c.card == card){
          c.copy(enabled = false)
        } else c
      }
    }
  }
}
