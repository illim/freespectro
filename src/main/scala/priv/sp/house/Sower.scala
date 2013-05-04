package priv.sp.house

import priv.sp._

trait Sower {
  import CardSpec._
  import GameCardEffect._

  val monsterPlant = Creature("MonsterPlant", Some(6), 21, "when kills creature, heals completely all monster plants on the board.", runAttack = new MonsterPlantAttack)

  val Sower = House("Sower", List(
    Spell("Tangling", "Transfers X health from target creature to opposite creature\n(X = attack of target creature)",
          inputSpec = Some(SelectTargetCreature),
          effects = effects(Direct -> tangle)),
    Spell("Devouring", "Sacrifices friendly creature,\ndoubles attack of the rest of friendly creatures for 1 turn.",
          inputSpec = Some(SelectOwnerCreature),
          effects = effects(Direct -> devour)),
    monsterPlant,
    Spell("Pollination", "turns target creature of X level into special creature of (X minus 3) level with full hp and\n heals X life to owner.",
          inputSpec = Some(SelectOwnerCreature),
          effects = effects(Direct -> pollinate)),
    Creature("BloodSundew", Some(6), 34, "when deals damage, regenerates the same amount of hp.", runAttack = new BloodSundewAttack),
    Creature("PredatorPlant", Some(6), 33, "when attacks creature, deals X additional damage to it\n(X = difference between its current and max hp).", runAttack = new PredatorPlantAttack),
    Creature("ForestDrake", Some(5), 55, "when owner summons special creature, creates its copy in nearest empty slot.", reaction = new ForestDrakeReaction),
    Creature("FieryFlower", Some(0), 35, "Every turn halves health of enemy creature with highest hp and\n gives 1 fire power to owner.\nWhen enters the game, deals to opponent X damage (X = his fire power)", effects = effects(OnTurn -> fieryFlower, Direct -> { env : Env =>
      env.otherPlayer.inflict(Damage(env.otherPlayer.getHouses(0).mana , isAbility = true))
    }))))

  Sower.initCards(Houses.basicCostFunc)

  private def tangle = { env : Env =>
    import env._
    val slot = otherPlayer.slots.value(selected)
    val damage = Damage(slot.attack, isSpell = true)
    otherPlayer.slots.inflictCreature(selected, damage)
    if (player.slots.value.isDefinedAt(selected)){
      player.slots.healCreature(selected, damage.amount)
    }
  }

  private def devour = { env : Env =>
    import env._
    player.slots.destroy(selected)
    val (newSlots, backup) = ((PlayerState.emptySlots, Map.empty[Int, Int]) /: player.slots.value){ case ((newslots, backup), (num, slot)) =>
      val old = (num, slot.attack)
      (newslots + (num -> slot.copy(attack = slot.attack * 2)), backup + old)
    }
    player.slots.write(newSlots)
    player.addEffect(OnEndTurn -> new RecoverAttack(backup))
  }

  private def pollinate : Effect = { env : Env =>
    import env._
    val slot = player.slots.value(selected)
    val cost = slot.card.cost
    Sower.cards.find(_.cost == cost - 3).foreach{ card =>
      card match {
        case c : Creature =>
          player.slots.destroy(selected)
          player.slots.summon(selected, c)
        case _ =>
      }
    }
    player.heal(cost)
  }

  private def fieryFlower = {env : Env =>
    import env._
    otherPlayer.slots.slots.toSeq.sortBy(_._2.life)(math.Ordering.Int.reverse).headOption foreach { case (num, slot) =>
      updater.focus(selected, playerId)
      otherPlayer.slots.inflictCreature(num, (Damage(math.ceil(slot.life / 2f).toInt, isAbility = true)))
    }
    player.houses.incrMana(1, 0)
  }

  private class MonsterPlantAttack extends Attack {
    def apply(num : Int, d : Damage, updater : GameStateUpdater, id : PlayerId) {
      val player = updater.players(id)
      val otherPlayer = player.otherPlayer
      otherPlayer.getSlots.get(num) match {
        case None => otherPlayer.inflict(d, Some(SlotSource(id, num)))
        case Some(_) =>
          otherPlayer.slots.inflictCreature(num, d)
          // FIXME maybe not good at all and should add source in damage?
          if (!otherPlayer.slots.value.isDefinedAt(num)){
            player.slots.value.foreach{ case (n, slot) =>
              if (slot.card == monsterPlant){
                player.slots.healCreature(n, monsterPlant.life)
              }
            }
          }
      }
    }
  }


  private class ForestDrakeReaction extends DefaultReaction {

    final override def onSummon(selected : Int, selectedPlayerId : PlayerId, summoned : SummonEvent) {
      import summoned._
      if (selectedPlayerId == playerId && selected != num && card.houseId == Sower.houseId){
        updater.focus(selected, playerId)
        val slots = updater.players(playerId).slots
        val dists = slotRange.collect{ case n if !slots.value.isDefinedAt(n) => (n, math.abs(n - selected)) }
        dists.sortBy(_._2).headOption.foreach{ case (pos, _) =>
          slots.add(pos, card)
        }
      }
    }
  }

}

// code horror
private class BloodSundewAttack extends Attack {

  def apply(num : Int, d : Damage, updater : GameStateUpdater, id : PlayerId) {
    val player = updater.players(id)
    val otherPlayer = player.otherPlayer
    val healAmount = otherPlayer.getSlots.get(num) match {
      case None =>
        otherPlayer.inflict(d, Some(SlotSource(id, num)))
        d.amount
      case Some(slot) =>
        val oldl = slot.life
        otherPlayer.slots.inflictCreature(num, d)
        val newl = otherPlayer.slots.value.get(num).map(_.life) getOrElse 0
        oldl - newl
    }
    if (player.slots.value.isDefinedAt(num)){ // horror due to zen eguard
      player.slots.healCreature(num, healAmount)
    }
  }
}

private class PredatorPlantAttack extends Attack {

  def apply(num : Int, d : Damage, updater : GameStateUpdater, id : PlayerId) {
    val player = updater.players(id)
    val otherPlayer = player.otherPlayer
    otherPlayer.getSlots.get(num) match {
      case None => otherPlayer.inflict(d, Some(SlotSource(id, num)))
      case Some(slot) =>
        val x = slot.card.life - slot.life
        otherPlayer.slots.inflictCreature(num, d.copy(amount = d.amount + x))
    }
  }
}

