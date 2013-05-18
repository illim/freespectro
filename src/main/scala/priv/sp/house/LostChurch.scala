package priv.sp.house

import priv.sp._
import priv.sp.update._

class LostChurch {
  import CardSpec._
  import GameCardEffect._

  val prisoner = Creature("Prisoner", Attack(2), 10, "When dying loose 1 mana of each basic houses.", reaction = new PrisonerReaction)
  val enragedPrisoner = Creature("EnragedPrisoner", Attack(8), 45, "Immune to spell & ability when liberator is alive.")
  val windOfOppression = Spell("WindOfOppression", "Stun scarecrow's opposite creature and its neighbours. Deals 7 damage to them", effects = effects(Direct -> oppress))
  val darkMonk = Creature("DarkMonk", Attack(2), 13, "Decrease opponent fire mana by 2\nand increase cost of them by 1 when alive.",
    effects = effects(Direct -> guardFire), reaction = new DarkMonkReaction)

  val LostChurch : House = House("LostChurch", List(
    Spell("SpeedDrug", "Add +1 attack to owner creatures, deals to them 4 damage.",
      effects = effects(Direct -> speedDrug)),
    Creature("Preacher", Attack(4), 15, "When in play normal cards cost 1 more mana.\nIncrease growth of special mana by 1.\nAdd 1 attack to prisoner",
      effects = effects(OnTurn -> addMana(1, 4), Direct -> preach), reaction = new PreacherReaction),
    Creature("FalseProphet", Attack(4), 18, "When in play normal cards cost 1 more mana.\nGive 2 mana to each basic house.\nTake one mana back when dying",
      reaction = new FalseProphetReaction, effects = effects(Direct -> prophetize)),
    Creature("AstralEscape", Attack(4), 30, "Damage done to prisoner is redirected to Astral escape", reaction = new AstralEscapeReaction),
    Creature("Scarecrow", Attack(8), 31, "Stuns&Deals 7 damage to opposite creature\nWhen dying heal opposite creature by 7.",
      effects = effects(Direct -> scare), reaction = new ScarecrowReaction),
    Spell("Madden", "Deals 10 damage to opponent creature and add everyone 1 attack.", effects = effects(Direct -> madden)),
    Creature("Falconer" , Attack(6), 35, "Each turns deals (slot distance) damage to opponent creatures.", effects = effects(OnTurn -> focus(falcon))),
    Creature("Liberator", Attack(4), 15, "Turns prisoner into Enraged prisoner.\n When dying inflict 15 damage to him.", reaction = new LiberatorReaction, effects = effects(Direct -> focus(deliverPrisoner)))),
    effects = List(OnEndTurn -> spawnPrisoner, OnTurn -> weaken))

  val preacher = LostChurch.cards(1)
  val falseProphet = LostChurch.cards(2)
  val scarecrow = LostChurch.cards(4)
  LostChurch.initCards(Houses.basicCostFunc)
  List(prisoner, enragedPrisoner, windOfOppression, darkMonk).foreach{ c =>
    c.houseIndex = LostChurch.houseIndex
    c.houseId = LostChurch.houseId
  }
  windOfOppression.cost = 3
  darkMonk.cost = 3

  val falseProphetAbility = Ability(falseProphet, darkMonk)
  val scarecrowAbility    = Ability(scarecrow, windOfOppression)

  def spawnPrisoner : Effect = { env : Env =>
    import env._
    if (!player.slots().exists{ case (n, slot) => slot.card == prisoner || slot.card == enragedPrisoner }){
      val emptySlots = player.slots.slots.filter(_.value.isEmpty)
      if (emptySlots.nonEmpty) {
        // todo deterministic random generator + exchange seed for multi
        val slot = emptySlots(scala.util.Random.nextInt(emptySlots.size))
        slot.add(prisoner)
        if (player.slots.findCard(preacher).isDefined){
          slot.attack.add(new PreacherAttackBonus)
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
      if (!slot.attack.has[PreacherAttackBonus]){
        slot.attack.add(new PreacherAttackBonus)
      }
    }
  }
  def preach = { env : Env =>
    import env._
    player.addDescMod(IncrBasicCostMod)
    giveHope(player)
  }
  class PreacherReaction extends DefaultReaction {
    final override def onMyDeath(dead : Dead){
      dead.player.removeDescMod(IncrBasicCostMod)
    }
  }
  def prophetize = { env : Env =>
    import env._
    player.houses.incrMana(2, 0, 1, 2, 3)
    player.addDescMod(IncrBasicCostMod, falseProphetAbility)
  }
  class FalseProphetReaction extends DefaultReaction {
    final override def onMyDeath(dead : Dead){
      dead.player.removeDescMod(IncrBasicCostMod)
      dead.player.houses.incrMana(-1, 0, 1, 2, 3)
    }
  }

  def scare = { env : Env =>
    val slot = env.otherPlayer.slots(env.selected)
    if (slot.value.isDefined){
      env.focus()
      slot.inflict(Damage(7, isAbility = true))
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
          oppSlot.inflict(Damage(7, isAbility = true))
          oppSlot.toggle(stunFlag)
        }
      }
    }
  }
  class ScarecrowReaction extends DefaultReaction {
    final override def onMyDeath(dead : Dead){
      dead.player.removeDescMod(scarecrowAbility)
      val slot = dead.player.otherPlayer.slots(dead.num)
      if (slot.value.isDefined){
        slot.heal(7)
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
    val bonus = AttackAdd(1)
    player.slots.inflictCreatures(Damage(4, isSpell = true))
    player.slots.foreach(_.attack.add(bonus))
  }
  def madden = { env : Env =>
    import env._
    otherPlayer.slots.foreach{ slot =>
      val d = Damage(10, isSpell = true)
      slot.inflict(d)
      if (slot.value.isDefined){
        slot.attack.add(new OneAttackBonus)
      }
    }
    player.slots.foreach{_.attack.add(new OneAttackBonus) }
  }
  def falcon = { env: Env =>
    import env._
    focus()
    otherPlayer.slots.foreach { slot =>
      if (slot.num != selected){
        slot.inflict(Damage(2 * math.abs(slot.num - selected), isAbility = true))
      }
    }
  }

  class AstralEscapeReaction extends DefaultReaction {
    final override def onProtect(selected : Int, d : DamageEvent) = {
      import d._
      var res = d.damage
      if (target.isDefined){
        val slot = player.slots(d.target.get)
        if (slot.get.card == prisoner){
          player.slots(selected).inflict(d.damage)
          res = d.damage.copy(amount = 0)
        }
      }
      res
    }
  }

  class LiberatorReaction extends DefaultReaction {
    final override def onMyDeath(dead : Dead){
      dead.player.slots.findCard(enragedPrisoner).foreach{ slot =>
        slot.inflict(Damage(15))
      }
    }

    final override def onProtect(selected : Int, d : DamageEvent) = {
      d.target match { // hack
        case Some(n) if selected != n && d.player.slots(n).get.card == enragedPrisoner && d.damage.isEffect =>
          d.damage.copy(amount = 0)
        case _ => d.damage
      }
    }
  }
}


class PrisonerReaction extends DefaultReaction {
  final override def onMyDeath(dead : Dead){
    dead.player.houses.incrMana(-1, 0, 1, 2, 3)
  }
}
class FalseProphetReaction extends DefaultReaction {
  final override def onMyDeath(dead : Dead){
    dead.player.houses.incrMana(-2, 0, 1, 2, 3)
  }
}
class DarkMonkReaction extends DefaultReaction {
  final override def onMyDeath(dead : Dead){
    dead.otherPlayer.removeDescMod(IncrFireCostMod)
  }
}
class OneAttackBonus extends AttackFunc { def apply(attack : Int) = attack + 1 }
class PreacherAttackBonus extends OneAttackBonus
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
