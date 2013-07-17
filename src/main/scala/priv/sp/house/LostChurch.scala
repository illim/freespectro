package priv.sp.house

import priv.sp._
import priv.sp.update._

/**
 * Introduced bullshit:
 * prisoner -> random not managed by AI
 */
object LostChurch {
  import CardSpec._
  import GameCardEffect._

  val prisoner = Creature("Prisoner", Attack(2), 10, "When dying loose 1 mana of each basic houses.", reaction = new PrisonerReaction)
  val enragedPrisoner = Creature("Enraged Prisoner", Attack(7), 35, "Immune to spell & ability when liberator is alive.", reaction = new PrisonerReaction)
  val windOfOppression = Spell("wind of oppression", "Stun scarecrow's opposite creature and its neighbours. Deals 5 damage to them", effects = effects(Direct -> oppress))
  val darkMonk = Creature("Dark monk", Attack(2), 13, "Decrease opponent fire mana by 2\nand increase cost of them by 1 when alive.",
    effects = effects(Direct -> guardFire), reaction = new DarkMonkReaction)
  val preacher = Creature("Preacher", Attack(4), 13, "When in play normal cards cost 1 more mana.\nIncrease growth of special mana by 1.\nAdd 1 attack to prisoner",
      effects = effects(OnTurn -> addMana(1, 4), Direct -> preach), reaction = new PreacherReaction)
  val falseProphet : Creature = Creature("false prophet", Attack(4), 18, "When in play normal cards cost 1 more mana.\nGive 2 mana to each basic house.\nTake one mana back when dying",
      reaction = new FalseProphetReaction, effects = effects(Direct -> prophetize))
  val astralEscape = Creature("Astral escape", Attack(4), 30, "Damage done to prisoner is redirected to Astral escape", reaction = new AstralEscapeReaction)
  val scarecrow : Creature = Creature("Scarecrow", Attack(8), 28, "Stuns&Deals 5 damage to opposite creature\nWhen dying heal opposite creature by 5.",
      effects = effects(Direct -> scare), reaction = new ScarecrowReaction)
  val liberator = Creature("Liberator", Attack(3), 15, "Turns prisoner into Enraged prisoner.\n When dying inflict 15 damage to him.", reaction = new LiberatorReaction, effects = effects(Direct -> focus(deliverPrisoner)))

  val LostChurch = new House("Lost Church", List(
    Spell("Speed drug", "Add +1 attack to owner creatures, deals to them 4 damage.\nEffect disappear when prisoner die.",
      effects = effects(Direct -> speedDrug)),
    preacher,
    falseProphet,
    astralEscape,
    scarecrow,
    liberator,
    Creature("Falconer" , Attack(6), 35, "Each turns deals (slot distance) damage to opponent creatures.", effects = effects(OnTurn -> focus(falcon))),
    Spell("Madden", "Deals 8 damage to opponent creature and add everyone 1 attack.", effects = effects(Direct -> madden))),
    effects = List(OnEndTurn -> spawnPrisoner, OnTurn -> weaken),
    eventListener = Some(new CustomListener(new LCEventListener)))

  val additionalCards = List(windOfOppression, darkMonk)

  LostChurch.initCards(Houses.basicCostFunc)
  List(prisoner, enragedPrisoner, windOfOppression, darkMonk).foreach{ c =>
    c.houseIndex = LostChurch.houseIndex
    c.houseId = LostChurch.houseId
  }
  windOfOppression.cost = 3
  windOfOppression.cardIndex = 4
  darkMonk.cost = 3
  darkMonk.cardIndex = 3
  prisoner.cost = 1
  enragedPrisoner.cost = 4

  val falseProphetAbility = Ability(falseProphet, darkMonk)
  val scarecrowAbility    = Ability(scarecrow, windOfOppression)

  def spawnPrisoner : Effect = { env : Env =>
    import env._
    if (!player.slots().exists{ case (n, slot) => slot.card == prisoner || slot.card == enragedPrisoner }){
      val openSlots = player.slots.getOpenSlots
      if (openSlots.nonEmpty) {
        val slot = openSlots(scala.util.Random.nextInt(openSlots.size))
        slot.add(prisoner)
        if (player.slots.findCard(preacher).isDefined){
          slot.attack.add(PreacherAttackBonus)
        }
        slot.focus(blocking = false)
      }
    }
  }
  def guardFire = { env : Env =>
    env.otherPlayer.houses.incrMana(-2 , 0)
    env.otherPlayer.addDescMod(IncrFireCostMod)
  }
  def weaken : Effect = { env : Env =>
    import env._
    player.slots().foreach{ case (num, slot) =>
      if (slot.card.houseId == LostChurch.houseId && slot.life < (slot.card.life / 2) && !slot.attackSources.sources.exists(_.isInstanceOf[LCAttack])) {
        player.slots(num).attack.add(LCAttack(- math.ceil(slot.card.attack.base.get / 3f).toInt))
      }
    }
  }
  def giveHope(player : PlayerUpdate) = {
    player.slots.findCard(prisoner).foreach{ slot =>
      if (!slot.attack.has[PreacherAttackBonus.type]){
        slot.attack.add(PreacherAttackBonus)
      }
    }
  }
  def preach = { env : Env =>
    import env._
    player.addDescMod(IncrBasicCostMod)
    giveHope(player)
  }
  class PreacherReaction extends Reaction {
    final override def onMyDeath(dead : Dead){
      import dead.player
      player.slots.findCard(prisoner).foreach{ slot =>
        if (player.slots.findCard(preacher).isEmpty){
          slot.attack.removeAny(PreacherAttackBonus)
        }
      }
      player.removeDescMod(IncrBasicCostMod)
    }
  }
  def prophetize = { env : Env =>
    import env._
    player.houses.incrMana(2, 0, 1, 2, 3)
    player.addDescMod(IncrBasicCostMod, falseProphetAbility)
  }

  def scare = { env : Env =>
    val slot = env.otherPlayer.slots(env.selected)
    if (slot.value.isDefined){
      env.focus()
      slot.inflict(Damage(5, env, isAbility = true))
      slot.toggle(stunFlag)
    }
    env.player.addDescMod(scarecrowAbility)
  }
  def oppress = { env : Env =>
    import env._
    player.slots.findCard(scarecrow).foreach{ slot =>
      slotInterval(slot.num - 1, slot.num + 1).foreach{ n =>
        val oppSlot = otherPlayer.slots(n)
        if (oppSlot.value.isDefined){
          oppSlot.inflict(Damage(5, env, isAbility = true))
          oppSlot.toggle(stunFlag)
        }
      }
    }
  }
  class ScarecrowReaction extends Reaction {
    final override def onMyDeath(dead : Dead){
      dead.player.removeDescMod(scarecrowAbility)
      val slot = dead.player.otherPlayer.slots(dead.num)
      if (slot.value.isDefined){
        slot.heal(5)
      }
    }
  }

  def deliverPrisoner = { env : Env =>
    import env._
    player.slots.value.find{ case (n, slot) => slot.card == prisoner }.foreach{ case (n, slot) =>
      player.slots(n).destroy()
      player.slots(n).add(enragedPrisoner)
    }
  }

  def speedDrug = { env : Env =>
    import env._
    val bonus = LCAttackBonus(env.player.id)
    player.slots.foreach(_.attack.add(bonus))
    player.slots.inflictCreatures(Damage(4, env, isSpell = true))
  }
  def madden = { env : Env =>
    import env._
    val bonus = AttackAdd(1)
    otherPlayer.slots.foreach{ slot =>
      val d = Damage(8, env, isSpell = true)
      slot.inflict(d)
      if (slot.value.isDefined){
        slot.attack.add(bonus)
      }
    }
    player.slots.foreach{_.attack.add(bonus) }
  }
  def falcon = { env: Env =>
    import env._
    focus()
    otherPlayer.slots.foreach { slot =>
      if (slot.num != selected){
        slot.inflict(Damage(math.abs(slot.num - selected), env, isAbility = true))
      }
    }
  }

  class AstralEscapeReaction extends Reaction {
    final override def onProtect(selected : SlotUpdate, d : DamageEvent) = {
      import d._
      var res = d.damage
      if (target.isDefined){
        val slot = player.slots(d.target.get)
        if (slot.get.card == prisoner){
          selected.inflict(d.damage)
          res = d.damage.copy(amount = 0)
        }
      }
      res
    }
  }

  class LiberatorReaction extends Reaction {
    final override def onMyDeath(dead : Dead){
      dead.player.slots.findCard(enragedPrisoner).foreach{ slot =>
        slot.inflict(Damage(15, Context(dead.player.id)))
      }
    }

    final override def onProtect(selected : SlotUpdate, d : DamageEvent) = {
      d.target match { // hack
        case Some(n) if selected.num != n && d.player.slots(n).get.card == enragedPrisoner && d.damage.isEffect =>
          d.damage.copy(amount = 0)
        case _ => d.damage
      }
    }
  }

  class PrisonerReaction extends Reaction {
    final override def onMyDeath(dead : Dead){
      if (dead.player.slots.findCard(liberator).isEmpty){
        dead.player.houses.incrMana(-1, 0, 1, 2, 3)
      }
      val bonus = LCAttackBonus(dead.player.id)
      dead.player.slots.foreach{_.attack.removeAny(bonus) }
    }
  }

  // crap
  class LCEventListener extends HouseEventListener {
    def protect(slot : SlotUpdate, damage : Damage) = {
      val target = slot.get.card
      if (target == prisoner || target == enragedPrisoner){
        player.slots.foldl(damage) { (acc, s) =>
          val sc = s.get.card
          if (sc == astralEscape || sc == liberator){
            s.get.card.reaction.onProtect(s, DamageEvent(acc, Some(slot.num), player))
          } else acc
        }
      } else damage
    }


    override def init(p : PlayerUpdate){
      super.init(p)
      p.slots.slots.foreach{ slot =>
        slot.protect.intercept(d => protect(slot, d))
      }
    }
  }
}

class DarkMonkReaction extends Reaction {
  final override def onMyDeath(dead : Dead){
    dead.otherPlayer.removeDescMod(IncrFireCostMod)
  }
}
class OneAttackBonus extends AttackFunc { def apply(attack : Int) = attack + 1 }
object PreacherAttackBonus extends OneAttackBonus
case class LCAttack(half : Int) extends AttackFunc { def apply(attack : Int) = attack + half }
case object IncrBasicCostMod extends DescMod {
  def apply(house : House, cards : Vector[CardDesc]) : Vector[CardDesc] = {
    if (house.houseIndex == 4) cards
    else cards.map(c => c.copy( cost = c.cost + 1))
  }
}
case object IncrFireCostMod extends DescMod {
  def apply(house : House, cards : Vector[CardDesc]) : Vector[CardDesc] = {
    if (house.houseIndex == 0) cards.map(c => c.copy( cost = c.cost + 1))
    else cards
  }
}

case class LCAttackBonus(player : PlayerId) extends AttackFunc { def apply(attack : Int) = attack + 1 }

class FalseProphetReaction extends Reaction {
    final override def onMyDeath(dead : Dead){
      dead.player.removeDescMod(IncrBasicCostMod)
      dead.player.houses.incrMana(-1, 0, 1, 2, 3)
    }
}
