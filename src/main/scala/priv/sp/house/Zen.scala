package priv.sp.house

import priv.sp._
import priv.sp.update._
import priv.sp.gui._
import GameCardEffect._
import CardSpec._

/**
 * Introduced bullshit:
 * electric guard -> added lot of useless check to see if a slot is not killed by guard
 */
class ZenMage {

  val Zen : House = House("Zen", List(
    new Creature("Elementesist", Attack(3), 12, "Deals damage to opposite card, and to all opposite card of same mana.", runAttack = new ElemAttack),
    new Creature("Redlight bringer", Attack(3), 15, "deals x additional damage to creatures on opposite and adjacent slots,\nwhere x is the number of owner adjacent creatures.", runAttack = new RedlightAttack),
    Spell("Focus", "Every owner card dedicate 50% of their attack to the focused creature.",
      inputSpec = Some(SelectTargetCreature),
      effects = effects(Direct -> focusSpell)),
    new Creature("Electric guard", Attack(3), 21, "deals 3 damage to creatures damaging owner.", reaction = new EGuardReaction),
    new Creature("Dreamer", Attack(5), 24, "When in play spell are summoned with one turn late butwith cost -2.", reaction = new DreamerReaction),
    new Creature("Mimic", Attack(6), 26, "When in play, creature are summoned with one turn late with cost -2,\n giving 3 life to owner.", reaction = new MimicReaction),
    new Creature("Spiral of light", Attack(3), 19, "each turn, heals 1,2,3,2,1 to self and 4 adjacent cards\ndeals 1,2,3,2,1 to 5 opposite creatures", effects = effects(OnTurn -> spiral), runAttack = new SpiralAttack),
    new Creature ("Zen Fighter", Attack(7), 31, "When summoned gives 3 water mana.\nZen Fighter receives 30% damage from spells and abilities", reaction = new ZFReaction, effects = effects(Direct -> focus(addMana(3, 1))))), eventListener = Some(new CustomListener(new ZenEventListener)))

  val eguard = Zen.cards(3)
  Zen.initCards(Houses.basicCostFunc)

  private val cocoon = new Creature("Cocoon", Attack(0), 13){
    cost = 0
    houseIndex = Zen.houseIndex
    houseId = Zen.houseId
  }

  private class RedlightAttack extends RunAttack {
    isMultiTarget = true

    def apply(target : List[Int], d : Damage, player : PlayerUpdate) {
      val num = target.head
      val otherPlayer = player.otherPlayer
      val bonus       = player.slots(num).adjacentSlots.count(_.value.isDefined)
      val targets     = if (bonus == 0) List(num) else (num -1 to num +1)

      targets.foreach { n =>
        otherPlayer.getSlots.get(n) match {
          case None => if (n == num) player.otherPlayer.inflict(d)
          case Some(oppositeSlot) =>
            val slot = otherPlayer.slots(n)
            if (n == num) {
              slot.inflict(d.copy(amount = d.amount + bonus))
            } else {
              slot.inflict(d.copy(amount = bonus))
            }
        }
      }
    }
  }

  private class SpiralAttack extends RunAttack {
    isMultiTarget = true

    def apply(target : List[Int], d : Damage, player : PlayerUpdate) {
      val num = target.head
      val otherPlayer = player.otherPlayer

      slotInterval(num - 2, num +2).foreach{ n =>
        val damage = d.copy(amount = d.amount - math.abs(num - n))
        val slot = otherPlayer.slots(n)
        if (slot.value.isDefined) slot.inflict(damage)
        else if (n == num) {
          otherPlayer.inflict(d)
        }
      }
    }
  }

  private def focusSpell = { env: Env =>
    import env._

    val factor = AttackFactor(0.5f)
    val amount = player.slots.foldl(0)((acc, x) => acc + math.ceil(x.get.attack / 2f).toInt)
    otherPlayer.slots(env.selected).inflict(Damage(amount, env, isSpell = true))
    player.slots.foreach(_.attack.add(factor))
    player.addEffect(OnEndTurn -> new RemoveAttack(factor))
  }

  private def spiral = {env: Env =>
    import env._

    slotInterval(selected - 2, selected +2).foreach{ num =>
      val amount = 3 - (selected - num)
      val slot = player.slots(num)
      if (slot.value.isDefined) slot.heal(amount)
    }
  }

  private class EGuardReaction extends Reaction {
    final override def onProtect(d : DamageEvent) = {
      import d._
      if (target.isEmpty) {
        damage.context.selectedOption.foreach{ num =>
          player.updater.focus(selected.num, player.id, blocking = false)
          player.updater.players(damage.context.playerId).slots(num).inflict(Damage(3, Context(player.id, Some(eguard), selected.num), isAbility = true))
        }
      }
      d.damage
    }
  }

  trait ZenReaction {
    def interceptSubmit(command : Command, updater : GameStateUpdater) : (Boolean, Option[Command])
  }

  private class DreamerReaction extends Reaction with ZenReaction {
    final def interceptSubmit(command : Command, updater : GameStateUpdater) = {
      if (command.card.isSpell && command.flag == None){
        val c = command.copy(flag = Some(DreamCommandFlag), cost = math.max(0, command.cost - 2))
        updater.players(command.player).addEffect(OnTurn -> new Dream(c))
        (true, None)
      } else (false, None)
    }
  }

 private class MimicReaction extends Reaction with ZenReaction {
    final def interceptSubmit(command : Command, updater : GameStateUpdater) = {
      if (!command.card.isSpell && command.flag == None){
        val c = command.copy(flag = Some(DreamCommandFlag), cost = math.max(0, command.cost - 2))
        updater.players(command.player).addEffect(OnTurn -> new Hatch(c))
        (true, Some(Command(command.player, cocoon, command.input, 0)))
      } else (false, None)
    }
  }

  private class Hatch(c : Command) extends Function[Env, Unit]{
    def apply(env : Env){
      if (c.card.inputSpec.exists{
        case SelectOwnerSlot =>
          env.player.slots().get(c.input.get.num).exists(_.card == cocoon)
        case _ => false
      }){
        env.player.heal(3)
        env.player.submit(Some(c))
      }
      env.player.removeEffect(_.isInstanceOf[Hatch])
    }
  }

  private class Dream(c : Command) extends Function[Env, Unit]{
    def apply(env : Env){
      if (! c.card.inputSpec.exists{
        case SelectOwner(_) => error("not managed!!!!")
        case SelectTarget(_) => error("not managed!!!!")
        case SelectOwnerSlot =>
          env.player.slots().isDefinedAt(c.input.get.num)
        case SelectOwnerCreature =>
          !env.player.slots().isDefinedAt(c.input.get.num)
        case SelectTargetSlot =>
          env.otherPlayer.slots().isDefinedAt(c.input.get.num)
        case SelectTargetCreature =>
          !env.otherPlayer.slots().isDefinedAt(c.input.get.num)
      }){
        env.player.submit(Some(c))
      }
      env.player.removeEffect(_.isInstanceOf[Dream])
    }
  }


  class ZFReaction extends Reaction {
      override def selfProtect(d : Damage) = {
        if (d.isEffect) d.copy(amount = math.ceil(0.3 * (d.amount)).intValue)
        else d
      }
  }

  class ZenEventListener extends HouseEventListener {
    override def interceptSubmit(commandOption : Option[Command]) : (Boolean, Option[Command]) = {
      commandOption match {
        case None => (false, None)
        case Some(c) =>
          player.slots.foldl((false, Option.empty[Command])) { (acc, s) =>
            if (acc._1) acc else {
              s.get.reaction match {
                case z : ZenReaction => z.interceptSubmit(c, player.updater)
                case _ => acc
              }
            }
          }
      }
    }
  }
}

object DreamCommandFlag extends CommandFlag

class ElemAttack extends RunAttack {
  def apply(target : List[Int], d : Damage, player : PlayerUpdate) {
    val num = target.head
    val otherPlayer = player.otherPlayer
    otherPlayer.getSlots.get(num) match {
      case None => otherPlayer.inflict(d)
      case Some(oppositeSlot) =>
        val h = oppositeSlot.card.houseIndex
        otherPlayer.slots.foreach{ s =>
          if (s.get.card.houseIndex == h){
            s.inflict(d)
          }
        }
    }
  }
}
