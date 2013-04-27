package priv.sp.house

import priv.sp._

trait JunkMage {
  import CardSpec._
  import GameCardEffect._

  private val trashCyborg = Creature("TrashCyborg", Some(3), 30, "Fill the board with trash 2/11 and one cyborg.\nEvery turn 2 pieces of trash assemble into the cyborg", effects = effects(Direct -> spawnTrash, OnTurn -> gatherTrash))

  val Junk : House = House("Junk", List(
    Creature("Screamer", Some(2), 13, "+1 attack for each screamer in play", slotEffect = new ScreamerSlotEffect),
    Spell("PoisonFlower", "Deals 5 damage to target and creatures around. Deals 3 damage to opponent.",
          inputSpec = Some(SelectTargetCreature),
          effects = effects(Direct -> poisonFlower)),
    Creature("ChainController", Some(4), 21, "When adjacent creature of cost <6 die,\n fill the slot with another weak creature nearby", reaction = new ChainControllerReaction),
    Creature("JunkyardGoddess", Some(4), 26, "Absorb 3 of first damage done to either owner or creature of cost < 6", reaction = new JunkyardGoddessReaction, effects = effects(OnEndTurn -> resetProtect), data = Boolean.box(false)),
    Creature("RoamingAssassin", Some(6), 27, "At end of turn, if unblocked, move to the closest next unblocked opponent\n and deals 5 damage to it", effects = effects(OnEndTurn -> roam)),
    Creature("Factory", Some(4), 29, "When spawning a card of cost < 6 near to it,\nit produce a mirror in the other adjacent slot", reaction = new FactoryReaction),
    Creature("RecyclingBot", Some(8), 29, "When owner creature die, heal 10 life. If his life is already full,\n heal the player with 2 life for each creature lost.", reaction = new RecyclingBotReaction),
    trashCyborg))

  Junk.initCards(Houses.basicCostFunc)

  private val trash = new Creature("Trash", Some(2), 11){
    cost = 1
    houseIndex = Junk.houseIndex
    houseId = Junk.houseId
  }

  private val screamer = Junk.cards(0)

  private def resetProtect = { env: Env =>
    env.player.slots.setData(env.selected, Boolean.box(false))
  }

  private def spawnTrash = { env: Env =>
    def spawnTrashAt(num : Int){
      if (env.player.slots.value.get(num).isEmpty && num > -1 && num < 6){
        env.player.slots.add(num, trash)
      }
    }
    slotRange.foreach(spawnTrashAt _)
  }

  private def gatherTrash : CardSpec.Effect = { env: Env =>
    val slots = env.player.slots
    val trashs = slots.value.filter(_._2.card == trash).take(2)
    trashs.foreach(x => slots.destroy(x._1))
    if (trashs.nonEmpty){
      val life = trashs.toList.map(_._2.life).sum
      val attack = trashs.size * trash.attack.get
      // get first !
      slots.value.find(_._2.card == trashCyborg).foreach{ case (num, slot) =>
        env.updater.focus(num, env.playerId)
        slots.update(_ + (num -> slot.copy(attack = slot.attack + attack, life = slot.life + life)))
      }
    }
  }

  private def roam = { env: Env =>
    val otherSlots = env.otherPlayer.slots.value
    val slots = env.player.slots.value
    if (! otherSlots.isDefinedAt(env.selected)){
      otherSlots.keys.collect{ case n if !slots.isDefinedAt(n) => n }.toList.sortBy(x => math.abs(x - env.selected)).headOption.foreach{ dest =>
        env.otherPlayer.slots.inflictCreature(dest, Damage(5, isAbility = true))
        env.player.slots.move(env.selected, dest)
      }
    }
  }

  private def poisonFlower = { env: Env =>
    import env._

    val damage = Damage(5, isSpell = true)
    (selected-1 to selected +1).foreach{ num =>
      if (otherPlayer.slots.value.isDefinedAt(num)) otherPlayer.slots.inflictCreature(num, damage)
      if (player.slots.value.isDefinedAt(num)) player.slots.inflictCreature(num, damage)
    }
    otherPlayer.inflict(Damage(3, isSpell = true))
  }

  private class ScreamerSlotEffect extends DefaultSlotEffect {
    final override def applySlots(selected : Int, slots : PlayerState.SlotsType) = {
      applyAttackBonus(selected, slots, 1)
    }

    final override def unapplySlots(selected : Int, slots : PlayerState.SlotsType) = {
      applyAttackBonus(selected, slots, -1)
    }

    private def applyAttackBonus(selected : Int, slots : PlayerState.SlotsType, fact : Int = 1) = {
      val nbScreamers = slots.count(_._2 .card == screamer)
      (slots /: slots){ case (acc, (n, slot)) =>
        acc + (n -> (if (slot.card == screamer){
          if (n == selected) {
            slot.copy(attack = slot.attack + fact * nbScreamers)
          } else {
            slot.copy(attack = slot.attack + fact * 1)
          }
        } else slot))
      }
    }
  }
}

class JunkyardGoddessReaction extends DefaultReaction {
  final override def onProtect(selected : Int, d : DamageEvent) = {
    import d._
    val playerUpdate = updater.players(playerId)
    val slot = playerUpdate.slots.value(selected)
    if (!slot.data.asInstanceOf[Boolean]
        && (d.target.isEmpty || playerUpdate.slots.value(d.target.get).card.cost < 6)){
        updater.focus(selected, playerId)
        playerUpdate.slots.setData(selected, Boolean.box(true))
        math.max(0, d.amount - 3)
    } else d.amount
  }
}

class ChainControllerReaction extends DefaultReaction {
  final override def onDeath(selected : Int, dead : Dead){
    import dead._
    val step = num - selected
    if (card.cost < 6 && math.abs(step) == 1){
      val playerUpdate = updater.players(playerId)
      (playerUpdate.slots.value.get(num + step) match {
        case Some(slot) if slot.card.cost < 6 => Some(num + step)
        case _ =>
          playerUpdate.slots.value.get(selected - step) match {
            case Some(slot) if slot.card.cost < 6 => Some(selected -step)
            case _ => None
          }
      }).foreach{ dest =>
        updater.focus(selected, playerId)
        playerUpdate.slots.move(dest, num)
      }
    }
  }
}

class FactoryReaction extends DefaultReaction {
  final override def onSummon(selected : Int, selectedPlayerId : PlayerId, summoned : SummonEvent) {
    import summoned._
    val step = selected - num
    if (selectedPlayerId == playerId
        && math.abs(step) == 1
        && card.cost < 6){
      updater.focus(selected, playerId)
      val slots = updater.players(playerId).slots
      val pos = selected + step
      if (!slots.value.isDefinedAt(pos) && pos > -1 && pos < 6){
        slots.add(pos, card)
      }
    }
  }
}

class RecyclingBotReaction extends DefaultReaction {
  final override def onDeath(selected : Int, dead : Dead){
    import dead._
    if (selected != num){
      val playerUpdate = updater.players(playerId)
      playerUpdate.slots.value.get(selected).foreach{ botSlot =>
        updater.focus(selected, playerId)
        if (botSlot.life == botSlot.card.life) {
          playerUpdate.heal(2)
        } else {
          playerUpdate.slots.healCreature(selected, 10)
        }
      }
    }
  }
}
