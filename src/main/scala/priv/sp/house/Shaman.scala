package priv.sp.house

import priv.sp._
import priv.sp.update._
import GameCardEffect._

object Shaman {
  import CardSpec._

  val wolf = Creature("ghost wolf", Attack(2), 18, reaction = new WolfReaction, runAttack = new WolfAttack,
                      effects = effects(Direct -> disableWolf))
  val shadow = Creature("Wolf shadow", Attack(4), 50, "all cards which affect wolf affect wolf shadow as well.\nWhen enters the game, its neighbours attack immediately.",
                        effects = effects(Direct -> shade),
                        runAttack = new WolfAttack)

  val Shaman : House = House("Shaman", List(
    Spell("Unappeasable Hunger", "wolf receives +X attack (X - attack of target creature) for 1 turn.",
          inputSpec = Some(SelectOwnerCreature),
          effects = effects(Direct -> hunger)),
    Creature("Spirit of rage", Attack(2), 10, "when enters the game, permanently increases attack of neighbours by 1.", effects = effects(Direct -> rage)),
    Spell("Power of full moon", "permanently decreases damage dealt to wolf by 1 and\nheals 8 life to his neighbours and owner.", effects = effects(Direct -> fullMoon)),
    Spell("Phantom fury", "Deals 8 damage to all enemy creatures and\n permanently increases wolf attack by 1\nfor each creature died this turn.", effects = effects(Direct -> phantomFury)),
    Creature("Spirit protector", Attack(4), 23, "while protector remains in the game, all damage received by owner\nwill be decreased by 2,\nand enemy spells will heal wolf instead of damaging.", reaction = new ProtectorReaction),
    Creature("Spirit hunter", Attack(6), 34, "while hunter remains in the game, wolf gets +2 attack and\nheals himself on the dealt damage when attacks.",
             reaction = new HunterReaction,
             effects = effects(Direct -> hunt)),
    shadow,
    Creature("Phantom mate", Attack(5), 29, "when enters the game, permanently decreases cost of wolf cards by 1.\nEvery turn wolf additionally attacks slot opposite to mate.",
             reaction = new MateReaction,
             effects = effects(Direct -> mate))),
    effects = List(OnStart -> initWolf),
    data = WolfState(), eventListener = Some(new CustomListener(new ShamanEventListener)))

  Shaman.initCards(Houses.basicCostFunc)

  def initWolf = { env : Env =>
    val openSlots = env.player.slots.getOpenSlots
    val slot = openSlots(scala.util.Random.nextInt(openSlots.size))
    slot.add(wolf)
    slot.focus(blocking = false)
  }

  def disableWolf = { env : Env =>
    env.player.removeDescMod(WolfMod)
  }

  def isWolf(card : Card) = card == wolf || card == shadow

  def findWolves(slots : SlotsUpdate) = {
    slots.foldl(List.empty[SlotUpdate]){ (acc, s) =>
      val card = s.get.card
      if (isWolf(card)){
        s :: acc
      } else acc
    }
  }

  def hunger = { env : Env =>
    import env._
    val slots = player.slots
    findWolves(slots).foreach{ s =>
      val bonus = AttackAdd(slots(selected).get.attack)
      s.attack.add(bonus)
      player.addEffect(OnEndTurn -> new RemoveAttack(bonus))
    }
  }

  def rage = { env : Env =>
    env.getSelectedSlot.filledAdjacents.foreach(_.attack.add(OneAttackBonus))
  }

  def fullMoon = { env : Env =>
    import env._
    player.updateData[WolfState](x => x.copy(protection = x.protection + 1))
    findWolves(player.slots).foreach(_.filledAdjacents.foreach(_.heal(8)))
    player.heal(8)
  }

  def phantomFury = { env : Env =>
    import env._
    val damage = Damage(8, Context(env.playerId, None, selected), isSpell = true)
    otherPlayer.slots.inflictCreatures(damage)
    val wolves = findWolves(player.slots).map(_.num)
    env.player.updateData[WolfState](_.copy(furyWolves = wolves))
    player.addEffect(OnEndTurn -> new RemoveFury)
  }

  val huntBonus = AttackAdd(2)
  def hunt = { env : Env =>
    env.player.updateData[WolfState](x => x.copy(hunting = x.hunting + 1))
    findWolves(env.player.slots).foreach(_.attack.add(huntBonus))
  }

  def shade = { env : Env =>
    env.getSelectedSlot.filledAdjacents.foreach{ slot =>
      slot.player.runSlot(slot.num, slot.get)
    }
  }

  def mate = { env : Env =>
    env.player.updateData[WolfState](x => x.copy(mates = x.mates + env.selected))
    env.player.addDescMod(DecrSpecialCostMod)
  }

  class HunterReaction extends Reaction {
      final override def onAdd(selected : SlotUpdate, slot : SlotUpdate) = {
        if (slot.get.card == wolf){
          slot.attack.add(huntBonus)
        }
      }

      final override def onRemove(selected : SlotUpdate, slot : SlotUpdate){
        if (slot.get.card == wolf) {
          slot.attack.removeFirst(huntBonus)
        }
      }
      final override def onMyRemove(slot : SlotUpdate, dead : Option[Dead]) = {
        findWolves(slot.slots).foreach{ s =>
          s.attack.removeFirst(huntBonus)
        }
        dead.foreach(_.player.updateData[WolfState](x => x.copy(hunting = x.hunting - 1)))
      }
  }

  class ProtectorReaction extends Reaction {
      override def onProtect(selected : SlotUpdate, d : DamageEvent) = {
        import d._
        if (target.isEmpty){
          damage.copy(amount = math.ceil(damage.amount / 2.0).intValue)
        } else {
          if (d.damage.isSpell){
            val slot = selected.slots(d.target.get)
            slot.value match {
              case Some(s) if isWolf(s.card)=>
                selected.focus(blocking = false)
                slot.heal(d.damage.amount)
                d.damage.copy(amount = 0)
              case None => d.damage
            }
          } else d.damage
        }
      }
  }

  class WolfReaction extends Reaction {
      override def selfProtect(d : Damage, slot : SlotUpdate) = {
        val wolfState = slot.player.value.data.asInstanceOf[WolfState]
        if (wolfState.protection != 0){
          d.copy(amount = math.max(0, d.amount - wolfState.protection))
        } else d
      }

      override def cleanUp(selected : Int, player : PlayerUpdate) {
        player.insertDescMod(WolfMod)
      }
  }

  val wolfDesc = CardDesc(wolf, 2, true)
  case object WolfMod extends DescMod {
    def apply(house : House, cards : Vector[CardDesc]) : Vector[CardDesc] = {
      if (house.houseIndex < 4) cards
      else wolfDesc +: cards.tail
    }
  }

  class ShamanEventListener extends HouseEventListener {
    def reactDead(dead : Dead){
      if (dead.player.id != player.id ){
        player.value.data.asInstanceOf[WolfState].furyWolves.foreach{ n =>
          val slot = player.slots(n)
          if (slot.value.isDefined) slot.attack.add(OneAttackBonus)
        }
      }
    }
    override def init(p : PlayerUpdate){
      super.init(p)
      p.otherPlayer.slots.onDead.after{ dead =>
        reactDead(dead)
      }
    }
  }
}


case class WolfState(
  protection : Int = 0,
  furyWolves : List[Int] = Nil,
  hunting : Int = 0,
  mates : Set[Int] = Set.empty)

class MateReaction extends Reaction {

  override def cleanUp(selected : Int, player : PlayerUpdate) {
    player.updateData[WolfState](x => x.copy(mates = x.mates - selected))
  }
}
class RemoveFury extends Function[Env, Unit]{
  def apply(env : Env){
    env.player.updateData[WolfState](_.copy(furyWolves = Nil))
  }
}

private class WolfAttack extends RunAttack with DamageAttack {

  def apply(target : Option[Int], d : Damage, player : PlayerUpdate) {
    val wolfState = player.value.data.asInstanceOf[WolfState]
    val num = target.get
    var healAmount = damageAndGet(num, d, player)
    if (wolfState.mates.size > 0) {
      wolfState.mates.foreach{ n =>
        healAmount += damageCreatureAndGet(n, d, player)
      }
    }
    if (wolfState.hunting > 0){
      player.slots(num).heal(healAmount)
    }
  }
}

case object DecrSpecialCostMod extends DescMod {
  def apply(house : House, cards : Vector[CardDesc]) : Vector[CardDesc] = {
    if (house.houseIndex < 4) cards
    else cards.map(c => c.copy( cost = math.max(0, c.cost - 1)))
  }
}

