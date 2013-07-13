package priv.sp.house

import priv.sp._
import priv.sp.update._

class JunkMage {
  import CardSpec._
  import GameCardEffect._

  private val trashCyborg = Creature("Trash Cyborg", Attack(3), 30, "Fill the board with trash 2/11 and one cyborg.\nEvery turn 2 pieces of trash assemble into the cyborg", effects = effects(Direct -> spawnTrash, OnTurn -> gatherTrash))

  val Junk : House = House("Junk", List(
    Creature("Screamer", AttackSources(Some(2), Vector(ScreamerAttackSource)), 14, "+1 attack for each screamer in play", reaction = new ScreamerReaction),
    Spell("Poison flower", "Deals 5 damage to target and creatures around.\nDeals -1 mana for opponent ones.",
          inputSpec = Some(SelectTargetCreature),
          effects = effects(Direct -> poisonFlower)),
    Creature("Junkyard fortune", Attack(3), 19, "Absorb 2 of first damage done to either owner or creature of cost <=3", reaction = new JFReaction, effects = effects(OnEndTurn -> resetProtect), data = Boolean.box(false)),
    Creature("Chain controller", Attack(4), 18, "Mirror spawn of adjacent creature of cost <4.\n When adjacent creature of cost <6 die,\n fill the slot with another weak creature nearby", reaction = new ChainControllerReaction),
    Creature("Roaming assassin", Attack(6), 27, "At end of turn, if unblocked, move to the closest next unblocked opponent\n and deals 5 damage to it", effects = effects(OnEndTurn -> roam)),
    Creature("Factory", Attack(4), 29, "Mirror spawn of adjacent creature of cost < 6\n(spawn effect applied once)\nIf mirror position is blocked, heal factory by 5", reaction = new FactoryReaction),
    Creature("Recycling Bot", Attack(8), 29, "When owner creature die, heal 10 life. If his life is already full,\n heal the player with 2 life for each creature lost.", reaction = new RecyclingBotReaction),
    trashCyborg), eventListener = Some(new CustomListener(new JunkEventListener)))

  val jf = Junk.cards(2).asCreature
  Junk.initCards(Houses.basicCostFunc)

  private val trash = new Creature("Trash", Attack(2), 11){
    cost = 1
    houseIndex = Junk.houseIndex
    houseId = Junk.houseId
  }

  private val screamer = Junk.cards(0)

  private def resetProtect = { env: Env =>
    env.player.slots(env.selected).setData(Boolean.box(false))
  }

  private def spawnTrash = { env: Env =>
    def spawnTrashAt(num : Int){
      val slot = env.player.slots(num)
      if (slot.value.isEmpty) {
        slot.add(trash)
      }
    }
    env.player.value.slotList.foreach(spawnTrashAt _)
  }

  private def gatherTrash : CardSpec.Effect = { env: Env =>
    val slots = env.player.slots
    // bugged with card moves
    val trashStates = (List.empty[SlotState] /: baseSlotRange){ (acc, i) =>
      val slot = slots(i)
      if (acc.size < 2 && slot.value.isDefined && slot.get.card == trash){
        val state = slot.get
        slot.destroy()
        state :: acc
      } else acc
    }
    if (trashStates.nonEmpty){
      val life = trashStates.map(_.life).sum
      val attack = trashStates.size * trash.attack.base.get
      // get first !
      slots.slots.find(x => x.value.isDefined && x.get.card == trashCyborg).foreach{ slot =>
        env.updater.focus(slot.num, env.playerId)
        val s = slot.get
        slot.write(Some(s.copy(life = s.life + life)))
        slot.attack.add(AttackAdd(attack))
      }
    }
  }

  private def roam = { env: Env =>
    import env._
    val otherSlots = otherPlayer.slots
    if (otherSlots(selected).value.isEmpty) {
      nearestSlotOpposed(selected, player).foreach{ n =>
        val slots = player.slots
        val dest = slots(n)
        dest.inflict(Damage(5, env, isAbility = true))
        slots.move(selected, n)
      }
    }
  }

  private def poisonFlower = { env: Env =>
    import env._

    val damage = Damage(5, env, isSpell = true)
    val houses = slotInterval(selected-1, selected +1).flatMap{ num =>
      player.slots(num).inflict(damage)
      val oppSlot = otherPlayer.slots(num)
      oppSlot.value.map { slot =>
        oppSlot.inflict(damage)
        slot.card.houseIndex
      }
    }.distinct
    otherPlayer.houses.incrMana(-1 , houses : _*)
  }

  private class ScreamerReaction extends Reaction {
    final override def onAdd(selected : SlotUpdate, slot : SlotUpdate) = {
      if (slot.get.card == screamer){
        setScreamerDirty(slot.slots)
      }
    }
    final override def onMyDeath(dead : Dead) = setScreamerDirty(dead.player.slots)
    def setScreamerDirty(slots : SlotsUpdate){
      slots.foreach{ s =>
        if (s.get.card == screamer){
          s.attack.setDirty()
        }
      }
    }
  }

  case object ScreamerAttackSource extends AttackStateFunc {
    def apply(attack : Int, player : PlayerUpdate) = {
      val nbScreamers = player.slots.slots.count{ s =>
        s.value.isDefined && s.get.card == screamer
      }
      attack + nbScreamers
    }
  }

  class JunkEventListener extends HouseEventListener with OwnerDeathEventListener {
    override def protect(slot : SlotUpdate, damage : Damage) = {
      player.slots.foldl(damage) { (acc, s) =>
        val sc = s.get.card
        if (sc == jf){
          s.get.card.reaction.onProtect(s, DamageEvent(acc, Some(slot.num), player))
        } else acc
      }
    }
  }
}

class JFReaction extends Reaction {
  final override def onProtect(selected : SlotUpdate, d : DamageEvent) = {
    import d._
    if (!selected.get.data.asInstanceOf[Boolean]
        && (d.target.isEmpty || player.slots(d.target.get).get.card.cost < 4)){
        player.updater.focus(selected.num, player.id, blocking = false)
        selected.setData(Boolean.box(true))
        d.damage.copy(amount = math.max(0, d.damage.amount - 2))
    } else d.damage
  }
}

trait MirrorSummon extends Reaction {
  def maxCost : Int
  var healIfNoMirror = 0
  final override def onSummon(selected : Int, selectedPlayerId : PlayerId, summoned : SummonEvent) {
    import summoned._
    val step = selected - num
    if (selectedPlayerId == player.id
        && math.abs(step) == 1
        && card.cost < maxCost + 1){
      val pos = selected + step
      if (player.value.isInSlotRange(pos)){
        val slot = player.slots(pos)
        if (slot.value.isEmpty){
          player.updater.focus(selected, player.id)
          slot.add(card)
        } else if (healIfNoMirror != 0){
          player.slots(selected).heal(healIfNoMirror)
        }
      }
    }
  }
}
class ChainControllerReaction extends MirrorSummon {
  val maxCost = 3
  final override def onDeath(selected : Int, playerId : PlayerId, dead : Dead){
    import dead._
    val step = num - selected
    if (card.cost < 6 && math.abs(step) == 1){
      def getAt(n : Int) = {
        if (player.value.isInSlotRange(n)){
          player.slots(n).value match {
            case Some(slot) if slot.card.cost < 6 => Some(n)
            case _ => None
          }
        } else None
      }

      (getAt(num + step) orElse getAt(selected - step)).foreach{ dest =>
        player.updater.focus(selected, player.id)
        player.slots.move(dest, num)
      }
    }
  }
}

class FactoryReaction extends MirrorSummon {
  val maxCost = 5
  healIfNoMirror = 5
}

class RecyclingBotReaction extends Reaction {
  final override def onDeath(selected : Int, playerId : PlayerId, dead : Dead){
    import dead._
    val selectedSlot = player.slots(selected)
    selectedSlot.value.foreach{ botSlot =>
      player.updater.focus(selected, player.id)
      if (botSlot.life == botSlot.card.life) {
        player.heal(2)
      } else {
        selectedSlot.heal(10)
      }
    }
  }
}
