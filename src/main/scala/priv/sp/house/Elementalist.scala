package priv.sp.house

import priv.sp._
import priv.sp.update._

class Elementalist {
  import CardSpec._
  import GameCardEffect._

  val Elementalist = House("Elementalist", List(
    Creature("Sylph", Attack(5), 15, "When enters the game, allows to play additional special card.\n+1 cost for each sylph on the board.", reaction = new SylphReaction, effects = effects(Direct -> sylphEffect)),
    Spell("Deep freeze", "Both players skip 1 turn and cannot use special cards in their next turn.", effects = effects(Direct -> freeze)),
    Creature("Salamander", Attack(5), 16, "If owner fire power is higher than opponent fire power,\ndeals to opponent 5 damage at the beginning of the turn.", effects = effects(OnTurn -> salamand)),
    Spell("Avalanche", "Deals 2X damage to enemy creatures (X = owner earth power),\nheals 2X life to owner and reduces owner earth power to 0.", effects = effects(Direct -> aval)),
    Spell("Incineration", "Destroys strongest enemy and weakest friendly creatures\n(calculated by health) both on board and in deck.", effects = effects(Direct -> incinerate)),
    Creature("ArchPhoenix", Attack(9), 20, "Fire cards heal him instead of dealing damage.", reaction = new ArchPhoenixReaction),
    Creature("Stone golem", Attack(7), 30, "Regenerates 4 life when blocked.\nReceives no damage from spells and creatures abilities when unblocked.", reaction = new SGReaction, effects = effects(OnTurn -> stoneGole)),
    Spell("Frost lightning", "Deals X damage to opponent\n(X = difference between his lowest power and owner highest power)\nand permanently blocks target slot.",
          inputSpec = Some(SelectTargetSlot),
          effects = effects(Direct -> frostLight))))

  val sylph = Elementalist.cards(0)
  Elementalist.initCards(Houses.basicCostFunc)

  def sylphEffect = { env : Env =>
    import env._
    player.addDescMod(IncrSylphCostMod)
    player.addDescMod(HideBasicMod)
    player.addTransition(WaitPlayer(env.playerId))
    player.addEffect(OnEndTurn -> UnMod(HideBasicMod))
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
      player.addDescMod(HideSpecialMod)
      player.addEffect(OnEndTurn -> UnMod(HideSpecialMod))
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
      otherPlayer.inflict(Damage(5, env, isAbility = true))
    }
  }

  def aval = { env : Env =>
    import env._
    val x = player.getHouses(3).mana
    otherPlayer.slots.inflictCreatures(Damage(2 * x, env, isSpell = true))
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
    val own = player.getHouses.reduceLeft((h1, h2) => if (h1.mana > h2.mana) h1 else h2 ).mana
    val x = math.max(0, own - opp)
    otherPlayer.inflict(Damage(x, env, isSpell = true))
    otherPlayer.blockSlot(selected)
  }

  class SylphReaction extends Reaction {
    final override def onMyDeath(dead : Dead) {
      dead.player.removeDescMod(IncrSylphCostMod)
    }
  }

  class SGReaction extends Reaction {
    override def selfProtect(d : Damage, slot : SlotUpdate) = {
      // FiXME: hack watch if unblocked at start of "transaction"! for mass damage and now for titan
      val player = slot.slots.player
      if (!player.updater.value.players(other(player.id)).slots.isDefinedAt(slot.num)
          && !player.otherPlayer.slots(slot.num).value.isDefined ){
        if (d.isEffect){
          d.copy(amount = 0)
        } else d
      } else d
    }
  }

  class ArchPhoenixReaction extends Reaction {
    override def selfProtect(d : Damage, slot : SlotUpdate) = {
      d.context.card match {
        case Some(c) if c.houseIndex == 0 =>
          slot.heal(d.amount)
          d.copy(amount = 0)
        case _ => d
      }
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
      else cards.filter{ c => c.card != card }
    }
  }
}
