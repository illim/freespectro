package priv.sp.house

import priv.sp._
import priv.sp.gui._
import GameCardEffect._
import CardSpec._

// TODO manage dreamer in bot and show in gui when effect toggled

trait ZenMage {

  val Zen : House = House("Zen", List(
    Creature("Elementesist", Some(3), 12, "Deals damage to opposite card, and to all opposite card of same mana.", runAttack = new ElemAttack),
    Creature("RedlightBringer", Some(3), 15, "deals x additional damage to creatures on opposite and adjacent slots,\nwhere x is the number of owner adjacent creatures.", runAttack = new RedlightAttack),
    Spell("Focus", "Every owner card dedicate 50% of their attack to the focused creature.",
      inputSpec = Some(SelectTargetCreature),
      effects = effects(Direct -> focus)),
    Creature("ElectricGuard", Some(3), 21, "deals 3 damage to creatures damaging opponent.", reaction = new EGuardReaction),
    Creature("Dreamer", Some(5), 24, reaction = new DreamerReaction),
    Creature("Mimic", Some(6), 26, reaction = new MimicReaction),
    Creature("SpiralOfLight", Some(3), 27, "each turn, heals 1,2,3,2,1 to self and 4 adjacent cards\ndeals 1,2,3,2,1 to 5 opposite creatures", effects = effects(OnTurn -> spiral), runAttack = new SpiralAttack),
    new ZenFighter))

  Zen.initCards(Houses.basicCostFunc)

  private def dream = new Spell("Dream", "summon spell in next turn with cost -2"){
    cost = 0
    houseIndex = Zen.houseIndex
    houseId = Zen.houseId
    commandMod = Some(new DreamCommandMod)
  }

  private def cocoon = new Creature("Cocoon", Some(0), 11, "summon creature in next turn with cost -2"){
    cost = 0
    houseIndex = Zen.houseIndex
    houseId = Zen.houseId
    commandMod = Some(new DreamCommandMod)
  }

  Zen.cards(4).asCreature.ability = Some(dream)
  Zen.cards(5).asCreature.ability = Some(cocoon)

  private class DreamCommandMod extends CommandMod {
    def updateRecorder(cr : CommandRecorder){
      if (cr.flag.isDefined) cr.flag = None
      else cr.flag = Some(DreamCommandFlag)
    }
  }

  private class ElemAttack extends Attack {
    def apply(num : Int, d : Damage, updater : GameStateUpdater, id : PlayerId) {
      val otherPlayer = updater.players(other(id))
      otherPlayer.getSlots.get(num) match {
        case None => otherPlayer.inflict(d)
        case Some(oppositeSlot) =>
          val h = oppositeSlot.card.houseIndex
          otherPlayer.slots.value.foreach{ case (n, s) =>
            if (s.card.houseIndex == h){
              otherPlayer.slots.inflictCreature(n, d)
            }
          }
      }
    }
  }

  private class RedlightAttack extends Attack {
    def apply(num : Int, d : Damage, updater : GameStateUpdater, id : PlayerId) {
      val player      = updater.players(id)
      val otherPlayer = player.otherPlayer
      val bonus       = List(num-1, num+1).filter(player.slots.value.isDefinedAt _)
      val targets     = if (bonus.isEmpty) List(num) else (num -1 to num +1)

      targets.foreach { n =>
        otherPlayer.getSlots.get(n) match {
          case None => player.otherPlayer.inflict(d)
          case Some(oppositeSlot) =>
            if (bonus.size == 0){
              otherPlayer.slots.inflictCreature(n, d)
            } else if (n == num) {
              otherPlayer.slots.inflictCreature(n, d.copy(amount = d.amount + bonus.size))
            } else {
              otherPlayer.slots.inflictCreature(n, d.copy(amount = bonus.size))
            }
        }
      }
    }
  }

  private class SpiralAttack extends Attack {
    def apply(num : Int, d : Damage, updater : GameStateUpdater, id : PlayerId) {
      val otherPlayer = updater.players(id).otherPlayer

      (num - 2 to num +2).foreach{ n =>
        val damage = d.copy(amount = d.amount - math.abs(num - n))
        if (otherPlayer.getSlots.isDefinedAt(n)) otherPlayer.slots.inflictCreature(n, damage)
        else if (n == num) {
          otherPlayer.inflict(d)
        }
      }
    }
  }

  private def focus = { env: Env =>
    import env._

    val damage = Damage(player.slots.value.toList.map(_._2.attack).sum / 2, isSpell = true)
    otherPlayer.slots.inflictCreature(env.selected, damage)
    val (newSlots, backup) = ((PlayerState.emptySlots, Map.empty[Int, Int]) /: player.slots.value){ case ((newslots, backup), (num, slot)) =>
      val old = (num, slot.attack)
      (newslots + (num -> slot.copy(attack = math.ceil(slot.attack/2f).toInt)), backup + old)
    }
    player.slots.write(newSlots)
    player.addEffect(OnEndTurn -> new FocusRecover(backup))
  }

  class FocusRecover(backup : Map[Int, Int]) extends Function[Env, Unit]{
    def apply(env : Env){
      env.player.slots.update{ s =>
        s.map{ case (num, slot) =>
          (num -> slot.copy(attack = backup(num)))
        }
      }
      env.player.removeEffect(_.isInstanceOf[FocusRecover])
    }
  }

  private def spiral = {env: Env =>
    import env._

    (selected-2 to selected +2).foreach{ num =>
      val amount = 3 - (selected - num)
      if (player.slots.value.isDefinedAt(num)) player.slots.healCreature(num, amount)
    }
  }

  private class EGuardReaction extends DefaultReaction {
    final override def onProtect(selected : Int, d : DamageEvent) : Int = {
      if (d.target.isEmpty){
        d.source.foreach{ src =>
          d.updater.focus(selected, d.playerId, blocking = false)
          d.updater.players(src.playerId).slots.inflictCreature(src.num, Damage(3, isAbility = true))
        }
      }
      d.amount
    }
  }

  private class DreamerReaction extends DefaultReaction {
    final override def interceptSubmit(command : Command, updater : GameStateUpdater) = {
      if (command.card.isSpell && command.flag == Some(DreamCommandFlag)){
        updater.players(command.player).addEffect(OnTurn -> new Dream(command))
        true
      } else false
    }
  }

 private class MimicReaction extends DefaultReaction {
    final override def interceptSubmit(command : Command, updater : GameStateUpdater) = {
      if (!command.card.isSpell && command.flag == Some(DreamCommandFlag)){
        updater.players(command.player).addEffect(OnTurn -> new Dream(command))
        true
      } else false
    }
  }

  class Dream(c : Command) extends Function[Env, Unit]{
    def apply(env : Env){
      println("submit dream " + c)
      if (! c.card.inputSpec.exists{
        case SelectOwnerSlot =>
          env.player.slots.value.isDefinedAt(c.input.get.num)
        case SelectOwnerCreature =>
          !env.player.slots.value.isDefinedAt(c.input.get.num)
        case SelectTargetCreature =>
          !env.otherPlayer.slots.value.isDefinedAt(c.input.get.num)
      }){
        env.player.submit(c.copy(flag = None, cost = math.max(0, c.cost - 2)))
      }
      env.player.removeEffect(_.isInstanceOf[Dream])
    }
  }
}

class ZenFighter extends Creature ("ZenFighter", Some(7), 31, "When summoned gives 3 water mana. Zen Fighter receives 30% damage from spells and abilities", effects = effects(Direct -> focus(addMana(3, 1)))) {

  override def inflict(damage : Damage, life : Int) = {
    if (damage.isEffect) (life - math.ceil(0.3 * (damage.amount))).toInt
    else life - damage.amount
  }
}

object DreamCommandFlag extends CommandFlag
