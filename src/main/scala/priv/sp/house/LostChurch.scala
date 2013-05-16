package priv.sp.house

import priv.sp._
import priv.sp.update._

class LostChurch {
  import CardSpec._
  import GameCardEffect._

  val prisoner = Creature("Prisoner", Attack(2), 9, "When dying loose 1 mana of each basic houses.", reaction = new PrisonerReaction)
  val enragedPrisoner = Creature("EnragedPrisoner", Attack(8), 45, "Immune to spell & ability when liberator is alive.")
  val sacrificeFlesh = Spell("SacrificeFlesh", "Deals to owner and opponent creatures, damage equals to owner creatures' costs.", effects = effects(Direct -> flesh))
  val windOfOppression = Spell("WindOfOppression", "Stun scarecrow's opposite creature and its neighbours.", effects = effects(Direct -> oppress))
  val darkMonk = Creature("DarkMonk", Attack(2), 13, "Decrease opponent fire mana by 1\nand increase cost of them by 1 when alive.",
    effects = effects(Direct -> guardFire), reaction = new DarkMonkReaction)
  val falconer = Creature("Falconer", Attack(6), 28, "Each turns deals (2 * slot distance) damage to opponent creatures.", effects = effects(OnTurn -> focus(falcon)))
  val skinnedBeast = new SkinnedBeast

  val LostChurch : House = House("LostChurch", List(
    Spell("SpeedDrug", "Add +1 attack to owner creatures, deals to them 4 damage.",
      effects = effects(Direct -> speedDrug)),
    Spell("WildJustice", "Deals (12 - attack) damage to each creature.",
      effects = effects(Direct -> wildJustice)),
    Creature("Preacher", Attack(5), 19, "When in play normal cards cost 1 more mana.\nIncrease growth of special mana by 1.\nAdd 1 attack to prisoner",
      effects = effects(OnTurn -> addMana(1, 4), Direct -> preach), reaction = new PreacherReaction),
    Creature("FalseProphet", Attack(5), 19, "When in play normal cards cost 1 more mana.\nGive 2 mana to each basic house.\nAdd 1 attack to prisoner\nIncrease cost of basic card by 1",
      reaction = new FalseProphetReaction, effects = effects(Direct -> prophetize)),
    skinnedBeast,
    Creature("Scarecrow", Attack(7), 26, "Deals 7 damage on opposite creature, when dying heal opposite creature by 7.",
      effects = effects(Direct -> scare), reaction = new ScarecrowReaction),
    Creature("AstralEscape", Attack(4), 40, "damage done to prisoner is redirected to Astral escape", reaction = new AstralEscapeReaction),
    Creature("Liberator", Attack(4), 15, "Turns prisoner into Enraged prisoner. When dying inflict 15 damage to him.", reaction = new LiberatorReaction, effects = effects(Direct -> focus(deliverPrisoner)))),
    effects = List(OnEndTurn -> spawnPrisoner, OnTurn -> weaken))

  val falseProphet = LostChurch.cards(3)
  val scarecrow = LostChurch.cards(5)
  val liberator = LostChurch.cards(7)
  LostChurch.initCards(Houses.basicCostFunc)
  List(prisoner, enragedPrisoner, windOfOppression, darkMonk, falconer).foreach{ c =>
    c.houseIndex = LostChurch.houseIndex
    c.houseId = LostChurch.houseId
  }
  sacrificeFlesh.cost = 2
  windOfOppression.cost = 3
  darkMonk.cost = 3
  falconer.cost = 6

  val falseProphetAbility = Ability(falseProphet, darkMonk)
  val liberatorAbility = Ability(liberator, falconer)
  val skinnedBeastAbility = Ability(skinnedBeast, sacrificeFlesh)
  val scarecrowAbility = Ability(scarecrow, windOfOppression)

  def spawnPrisoner : Effect = { env : Env =>
    import env._
    if (!player.slots().exists{ case (n, slot) => slot.card == prisoner || slot.card == enragedPrisoner }){
      val emptySlots = player.slots.slots.filter(_.value.isEmpty)
      if (emptySlots.nonEmpty) {
        // todo deterministic random generator + exchange seed for multi
        val slot = emptySlots(scala.util.Random.nextInt(emptySlots.size))
        slot.add(prisoner)
        slot.focus(blocking = false)
      }
    }
  }
  def guardFire = { env : Env =>
    env.otherPlayer.houses.incrMana(-1 , 0)
    env.otherPlayer.addDescMod(IncrFireCostMod)
  }
  def weaken : Effect = { env : Env =>
    import env._
    player.slots().foreach{ case (num, slot) =>
      if (slot.card.houseId == LostChurch.houseId && slot.life < (slot.card.life / 2) && !slot.attackSources.sources.exists(_.isInstanceOf[LCAttack])) {
        player.slots(num).attack.add(LCAttack(- slot.card.attack.base.get / 3))
      }
    }
  }
  def giveHope(player : PlayerUpdate) = {
    player.slots.findCard(prisoner).foreach{ slot =>
      slot.attack.add(new HopeAttackBonus)
    }
  }
  skinnedBeast.effects = effects(Direct -> addDescMod(skinnedBeastAbility))
  skinnedBeast.reaction = new SkinnedBeastReaction
  class SkinnedBeastReaction extends DefaultReaction{
    final override def onMyDeath(dead : Dead){
      dead.updater.players(dead.playerId).removeDescMod(skinnedBeastAbility)
    }
  }
  def flesh = { env : Env =>
    import env._
    val otherSlots = otherPlayer.slots.slots
    player.slots.foreach{ s =>
      val d = Damage(s.get.card.cost, isSpell = true)
      s.inflict(d)
      val oppSlot = otherSlots(s.num)
      oppSlot.inflict(d)
    }
  }
  def preach = { env : Env =>
    import env._
    player.addDescMod(IncrBasicCostMod)
    giveHope(player)
  }
  class PreacherReaction extends DefaultReaction {
    final override def onMyDeath(dead : Dead){
      import dead._
      updater.players(playerId).removeDescMod(IncrBasicCostMod)
    }
  }
  def prophetize = { env : Env =>
    import env._
    player.houses.incrMana(2, 0, 1, 2, 3)
    player.addDescMod(IncrBasicCostMod, falseProphetAbility)
    giveHope(player)
  }
  class FalseProphetReaction extends DefaultReaction {
    final override def onMyDeath(dead : Dead){
      import dead._
      val player = updater.players(playerId)
      player.removeDescMod(IncrBasicCostMod)
    }
  }

  def scare = { env : Env =>
    val slot = env.otherPlayer.slots(env.selected)
    if (slot.value.isDefined){
      env.focus()
      slot.inflict(Damage(7, isAbility = true))
    }
    env.player.addDescMod(scarecrowAbility)
  }
  def oppress = { env : Env =>
    import env._
    player.slots.findCard(scarecrow).foreach{ slot =>
      slotInterval(slot.num - 1, slot.num + 1).foreach{ n =>
        val oppSlot = otherPlayer.slots(n)
        if (oppSlot.value.isDefined){
          oppSlot.inflict(Damage(8, isAbility = true))
          oppSlot.toggle(stunFlag)
        }
      }
    }
  }
  class ScarecrowReaction extends DefaultReaction {
    final override def onMyDeath(dead : Dead){
      import dead._
      val player = updater.players(playerId)
      player.removeDescMod(scarecrowAbility)
      val slot = player.otherPlayer.slots(num)
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
    player.addDescMod(liberatorAbility)
  }

  def speedDrug = { env : Env =>
    import env._
    val bonus = AttackAdd(1)
    player.slots.inflictCreatures(Damage(4, isSpell = true))
    player.slots.foreach(_.attack.add(bonus))
  }

  def wildJustice = { env : Env =>
    import env._
    def onPlayer(p : PlayerUpdate){
      p.slots.slots.foreach{ slot =>
        if (slot.value.isDefined){
          slot.inflict(Damage(math.max(0, 12 - slot.get.attack), isSpell = true))
        }
      }
    }
    onPlayer(player)
    onPlayer(otherPlayer)
  }

  def falcon = { env: Env =>
    import env._
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
      if (d.target.isDefined){
        val player = updater.players(playerId)
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
      import dead._
      val player = updater.players(playerId)
      player.slots.findCard(enragedPrisoner).foreach{ slot =>
        slot.inflict(Damage(15))
      }
    }

    final override def onProtect(selected : Int, d : DamageEvent) = {
      import d._
      val player = updater.players(playerId)
      d.target match { // hack
        case Some(n) if selected != n && player.slots(n).get.card == enragedPrisoner && d.damage.isEffect =>
          d.damage.copy(amount = 0)
        case _ => d.damage
      }
    }
  }
}

class SkinnedBeast extends Creature("SkinnedBeast", Attack(7), 28, "Damage done to him is transformed into (10 - damage)."){
  override def inflict(damage : Damage, life : Int) = super.inflict(damage.copy(amount = math.max(0, 10 - damage.amount)), life)
}
class PrisonerReaction extends DefaultReaction {
  final override def onMyDeath(dead : Dead){
    import dead._
    updater.players(playerId).houses.incrMana(-1, 0, 1, 2, 3)
  }
}
class FalseProphetReaction extends DefaultReaction {
  final override def onMyDeath(dead : Dead){
    import dead._
    updater.players(playerId).houses.incrMana(-2, 0, 1, 2, 3)
  }
}
class DarkMonkReaction extends DefaultReaction {
  final override def onMyDeath(dead : Dead){
    import dead._
    updater.players(other(playerId)).removeDescMod(IncrFireCostMod)
  }
}
class HopeAttackBonus extends AttackFunc { def apply(attack : Int) = attack + 1 }
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
