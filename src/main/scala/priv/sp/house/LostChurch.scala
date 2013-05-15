package priv.sp.house

import priv.sp._
import priv.sp.update._

trait LostChurch {
  import CardSpec._
  import GameCardEffect._

  private val prisoner = Creature("Prisoner", Attack(3), 7, "When dying loose 1 mana of each basic houses.", reaction = new PrisonerReaction)
  private val enragedPrisoner = Creature("EnragedPrisoner", Attack(8), 45, "Immune to spell & ability when liberator is alive.")
  private val windOfOppression = Spell("WindOfOppression", "Stun scarecrow's opposite creature and its neighbours.", effects = effects(Direct -> oppress))

  val LostChurch = House("LostChurch", List(
    Spell("SpeedDrug", "Add +1 attack to owner creatures, deals to them 3 damage.",
          effects = effects(Direct -> speedDrug)),
    Creature("Preacher", Attack(6), 19, "When in play normal cards cost 1 more mana.\nIncrease growth of special mana by 1.", effects = effects(OnTurn -> addMana(1, 4), Direct -> preachCost), reaction = new PreacherReaction),
    Spell("WildJustice", "Deals (12 - attack) damage to each creature.",
          effects = effects(Direct -> wildJustice)),
    Creature("FalseProphet", Attack(5), 19, "Give 2 mana to each basic house,\ntakes them back when dying.", effects = effects(Direct -> addMana(2, 0, 1, 2, 3))),
    Creature("Scarecrow", Attack(8), 26, "Deals 8 damage on opposite creature, when dying heal opposite creature by 5.", effects = effects(Direct -> scare)),
    new SkinnedBeast,
    Creature("Falconer", Attack(6), 28, "Each turns deals (2 * slot distance) damage to highest cost creature\n and creatures between.", effects = effects(OnTurn -> focus(falcon))),
    Creature("Liberator", Attack(4), 15, "Turns prisoner into Enraged prisoner. When dying inflict 15 damage to him.", reaction = new LiberatorReaction, effects = effects(Direct -> focus(deliverPrisoner)))),
    effects = List(OnEndTurn -> spawnPrisoner, OnTurn -> weaken))

  private val scarecrow = LostChurch.cards(4)
  LostChurch.initCards(Houses.basicCostFunc)
  List(prisoner, enragedPrisoner, windOfOppression).foreach{ c =>
    c.houseIndex = LostChurch.houseIndex
    c.houseId = LostChurch.houseId
  }
  windOfOppression.cost = 2

  private def spawnPrisoner : Effect = { env : Env =>
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

  private def weaken : Effect = { env : Env =>
    import env._
    player.slots().foreach{ case (num, slot) =>
      if (slot.card.houseId == LostChurch.houseId && slot.life < (slot.card.life / 2) && !slot.attackSources.sources.exists(_.isInstanceOf[LCAttack])) {
        player.slots(num).attack.add(LCAttack(- slot.card.attack.base.get / 4))
      }
    }
  }

  private def preachCost  = { env : Env => env.player.addDescMod(PreacherMod)  }
  case object PreacherMod extends DescMod {
    def apply(house : House, cards : Vector[CardDesc]) : Vector[CardDesc] = {
      if (house == LostChurch) cards
      else {
        cards.map(c => c.copy( cost = c.cost + 1))
      }
    }
  }

  class PreacherReaction extends DefaultReaction {
    final override def onDeath(selected : Int, dead : Dead){
      import dead._
      if (selected == num){
        updater.players(playerId).removeDescMod(PreacherMod)
      }
    }
  }

  private def scare = { env : Env =>
    env.focus()
    val slot = env.otherPlayer.slots(env.selected)
    if (slot.value.isDefined){
      slot.inflict(Damage(10, isAbility = true))
      slot.toggle(stunFlag)
    }
    env.player.addDescMod(ScareMod)
  }
  case object ScareMod extends DescMod {
    def apply(house : House, cards : Vector[CardDesc]) : Vector[CardDesc] = {
      if (house == LostChurch){
        cards.map{ c =>
          if (c.card == scarecrow){
            CardDesc(windOfOppression)
          } else c
        }
      } else cards
    }
  }
  private def oppress = { env : Env =>
    import env._
    findCard(player, _ == scarecrow).foreach{ slot =>
      slotInterval(slot.num - 1, slot.num + 1).foreach{ n =>
        val oppSlot = otherPlayer.slots(n)
        if (oppSlot.value.isDefined){
          oppSlot.inflict(Damage(8, isAbility = true))
          oppSlot.toggle(stunFlag)
        }
      }
    }
  }

  private def deliverPrisoner = { env : Env =>
    import env._
    player.slots.value.find{ case (n, slot) => slot.card == prisoner }.foreach{ case (n, slot) =>
      player.slots(n).destroy()
      player.slots(n).add(enragedPrisoner)
    }
  }

  private def speedDrug = { env : Env =>
    import env._
    val bonus = AttackAdd(1)
    player.slots.inflictCreatures(Damage(3, isSpell = true))
    player.slots.foreach(_.attack.add(bonus))
  }

  private def wildJustice = { env : Env =>
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

  private def falcon = { env: Env =>
    import env._

    otherPlayer.slots().toSeq.filter(_._1 != selected).sortBy(_._2.card.cost).lastOption foreach { case (num, slot) =>
      for(n <- selected to num by math.signum(num - selected)){
        otherPlayer.slots(n).inflict(Damage(2 * math.abs(n - selected), isAbility = true))
      }
    }
  }

  private def findCard(player : PlayerUpdate, f : Creature => Boolean) : Option[SlotUpdate] = {
    player.slots.slots.find{ s =>
      s.value.isDefined && f(s.get.card)
    }
  }

  class LiberatorReaction extends DefaultReaction {
    final override def onDeath(selected : Int, dead : Dead){
      import dead._
      if (selected == num){
        val player = updater.players(playerId)
        findCard(player, _ == enragedPrisoner).foreach{ slot =>
          slot.inflict(Damage(15))
        }
      }
    }

    final override def onProtect(selected : Int, d : DamageEvent) = {
      import d._
      val player = updater.players(playerId)
      d.target match {
        case Some(n) if player.slots.value(n).card == enragedPrisoner && d.damage.isEffect =>
          d.damage.copy(amount = 0)
        case _ => d.damage
      }
    }
  }
}

class SkinnedBeast extends Creature("SkinnedBeast", Attack(8), 31, "Damage done to him is transformed into (10 - damage)."){
  override def inflict(damage : Damage, life : Int) = super.inflict(damage.copy(amount = math.max(0, 10 - damage.amount)), life)
}

class PrisonerReaction extends DefaultReaction {
  final override def onDeath(selected : Int, dead : Dead){
    import dead._
    if (selected == num){
      updater.players(playerId).houses.incrMana(-1, 0, 1, 2, 3)
    }
  }
}

class FalseProphetReaction extends DefaultReaction {
  final override def onDeath(selected : Int, dead : Dead){
    import dead._
    if (selected == num){
      updater.players(playerId).houses.incrMana(-2, 0, 1, 2, 3)
    }
  }
}

case class LCAttack(half : Int) extends AttackFunc { def apply(attack : Int) = attack + half }
