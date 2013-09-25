package priv.sp.house

import priv.sp._
import priv.sp.update._
import CardSpec._
import GameCardEffect._

class SB {

  val deathLetter = new Creature("Death letter", Attack(0), 3, "reduce damage to 1. When die deals 20 damage to opposite creature", reaction = new DLReaction)
  val maiko = new Creature("Maiko", Attack(3), 13,
"""Decrease by 1 attack of all creatures on board.
(Replaced by death letter after summoned)""", effects = effects(Direct -> maikoEffect), reaction = new MaikoReaction)

  val SB = House("Snowblood", List(
    new Creature("Tracker", Attack(4), 14, "When alive, next summoned creature is invincible one turn.", reaction = new TrackerReaction, data = java.lang.Boolean.FALSE),
    new Creature("Bounty hunter", Attack(5), 20, "when opposite creature die give 1/3 mana of his cost round up", reaction = new BountyHunterReaction),
    maiko,
    Spell("Echo",
"""Each owner creature either triggers his 'each turn' effects
or deals damage equal to his attack/2 if he doesn't have any""", effects = effects(Direct -> echo)),
    new Creature("Kojiro", Attack(5), 27,
"""Can only be summoned onto an existing creature.
Kojiro attack the turn he is summoned
Each turn deals 2 damage to opposite&aligned creatures.""", status = runFlag, effects = effects(OnTurn -> kojiro), inputSpec = Some(SelectOwnerCreature)),
    new Creature("War guide", Attack(5), 26,
"""When next owner creature is summoned,
deals damage equals to his attack to opponent and his creatures.
Heal 1 life to aligned creatures when a creature is summoned in the pack""", reaction = new GuideReaction, data = java.lang.Boolean.FALSE),
    new Creature("Janus", Attack(7), 33,
"""each turn drain 2 life from other side of the board.
For each life drained, give one mana of the creature
if there is a symetric creature heal him by 1
or else heal the owner by 1""", effects = effects(OnTurn -> janus)),
    new Creature("Amaterasu", Attack(7), 32, """When summoned, and when a creature is summoned apply Amaterasu rules
if fire deals 4 damage to opposite creature
if water increase lowest mana by 2
if air deals 2 damage to opponent
if earth heal 3 life to owner""", effects = effects(Direct -> amaterasu), reaction = new AmaterasuReaction)),
                 eventListener = Some(new CustomListener(new SBEventListener)))


  SB.initCards(Houses.basicCostFunc)
  val additionalCards = List(deathLetter)
  additionalCards.foreach{ c =>
    c.houseIndex = SB.houseIndex
    c.houseId = SB.houseId
  }
  deathLetter.cost = 3
  val maikoAbility = Ability(maiko, deathLetter)

  private def echo = { env: Env =>
    import env._
    player.slots.foreach{ s =>
      val c = s.get.card
      c.effects(CardSpec.OnTurn) match {
        case None =>
          s.oppositeSlot.inflict(Damage(math.ceil(s.get.attack/2f).toInt, env, isSpell = true))
        case Some(f) =>
          val env = new GameCardEffect.Env(playerId, updater)
          env.card = Some(c)
          env.selected = s.num
          f(env)
      }
    }
  }

  def maikoEffect : Effect = { env : Env =>
    import env._
    val malus = MaikoMalus(selected)
    def temper(s : SlotUpdate) {
      s.attack.add(malus)
    }
    player.slots.foreach(temper)
    otherPlayer.slots.foreach(temper)
    player.addDescMod(maikoAbility)
  }

  class MaikoReaction extends Reaction {
      final override def onAdd(slot : SlotUpdate) = {
        if (slot != selected){
          val malus = MaikoMalus(selected.num)
          slot.attack.add(malus)
        }
      }
      final override def onRemove(slot : SlotUpdate) = {
        val malus = MaikoMalus(selected.num)
        slot.attack.removeFirst(malus)
      }
      final override def cleanUp(){
        val malus = MaikoMalus(selected.num)
        def removeMalus(s : SlotUpdate){ s.attack.removeFirst(malus) }
        selected.player.slots.foreach(removeMalus)
        selected.otherPlayer.slots.foreach(removeMalus)
      }
  }

  class DLReaction extends Reaction {
      override def inflict(damage : Damage){
        super.inflict(damage.copy(amount = 1))
      }

      override def onMyDeath(dead : Dead) {
        val d = Damage(20, Context(selected.playerId, Some(dead.card), selected.num), isAbility = true)
        selected.oppositeSlot.inflict(d)
      }
  }

  private def kojiro = { env: Env =>
    import env._
    val aligneds = getAligneds(otherPlayer.slots, selected)
    if (aligneds.nonEmpty){
      getSelectedSlot.focus()
      val d = Damage(2, env, isAbility = true)
      aligneds.foreach{ s =>
        s.inflict(d)
      }
    }
  }

  def getAligneds(slots : SlotsUpdate, selected : Int) = {
    val (aligneds, found, _) = slots.slots.foldLeft((List.empty[SlotUpdate], false, false)){ case (old @ (acc, found, end), s) =>
      if (end) old else {
        if (s.value.isEmpty){
          if (found) (acc, true, true) else (Nil, false, false)
        } else {
          (s :: acc, (s.num == selected) || found, false)
        }
      }
    }
    if (found) aligneds else Nil
  }

  class GuideReaction extends Reaction {
      final override def onAdd(slot : SlotUpdate) = {
        if (selected != slot){
          if(!selected.get.data.asInstanceOf[Boolean]){
            val d = Damage(slot.get.attack, Context(slot.playerId, Some(slot.get.card), slot.num), isAbility = true)
            selected.otherPlayer.slots.inflictCreatures(d)
            selected.setData(java.lang.Boolean.TRUE)
          }
          val aligneds = getAligneds(selected.slots, selected.num)
          if (aligneds contains (slot)){
            aligneds.foreach(_.heal(1))
          }
        }
      }
  }


  private def janus = { env: Env =>
    import env._
    val filleds = player.slots.filleds
    val (draineds, healeds) = if (selected > 2) {
      filleds.partition(_.num < 3)
    } else {
      filleds.partition(_.num > 2)
    }
    val d = Damage(2, env, isAbility = true)
    if (draineds.nonEmpty){
      getSelectedSlot.focus()
    }
    draineds.foreach{ s =>
      player.houses.incrMana(1, s.get.card.houseIndex)
      s.drain(d)
      healeds.find(_.num == 5 - s.num) match {
        case Some(h) => h.heal(1)
        case None => player.heal(1)
      }
    }
  }

  def applyAmaterasuRules(selected : SlotUpdate, slot : SlotUpdate) {
    slot.get.card.houseIndex match {
      case 0 =>
        val d = Damage(4, Context(selected.playerId, Some(selected.get.card), selected.num), isAbility = true)
        slot.oppositeSlot.inflict(d)
      case 1 =>
        selected.player.value.houses.zipWithIndex.sortBy(_._1.mana).headOption.foreach{ case (_, idx) =>
          selected.player.houses.incrMana(2, idx)
        }
      case 2 =>
        val d = Damage(2, Context(selected.playerId, Some(selected.get.card), selected.num), isAbility = true)
        selected.otherPlayer.inflict(d)
      case 3 =>
        selected.player.heal(3)
      case _ => ()
    }
  }

  def amaterasu = { env : Env =>
    import env._
    val selected = getSelectedSlot
    player.slots.foreach{ s => applyAmaterasuRules(selected, s) }
  }

  class AmaterasuReaction extends Reaction {
      final override def onAdd(slot : SlotUpdate) = {
        applyAmaterasuRules(selected, slot)
      }
  }

  class SBEventListener extends HouseEventListener with AnyDeathEventListener {
    def onEnemyAdd(slot : SlotUpdate) {
      player.slots.foreach{ s =>
        s.get.reaction match {
          case mk : MaikoReaction => mk.onAdd(slot)
          case _ => ()
        }
      }
    }

    override def init(p : PlayerUpdate){
      super.init(p)
      p.otherPlayer.slots.slots.foreach{ slot =>
        slot.add.after{ _ => onEnemyAdd(slot) }
      }
    }
  }
}

class BountyHunterReaction extends Reaction {
  final override def onDeath(dead : Dead){
    if (dead.player.id != selected.playerId && dead.num == selected.num){
      selected.player.houses.incrMana(math.ceil(dead.card.cost / 3f).toInt, dead.card.houseIndex)
    }
  }
}

class TrackerReaction extends Reaction {
    final override def onAdd(slot : SlotUpdate) = {
      if(!selected.get.data.asInstanceOf[Boolean] && selected != slot){
        slot.toggle(invincibleFlag)
        slot.player.addEffect(OnTurn -> RemoveInvincible(slot.get.id))
        selected.setData(java.lang.Boolean.TRUE)
      }
    }
}

case class MaikoMalus(id : Int) extends AttackFunc { def apply(attack : Int) = math.max(0, attack -1) }