package priv.sp.house

import priv.sp._
import priv.sp.update._

class DarkPriest {
  import CardSpec._
  import GameCardEffect._

  val restlessSoul = Creature("RestlessSoul", Attack(3), 11, "If dies, reborns at the end of opponent turn and\ngives 2 special mana to dark priest.", reaction = new RestlessReaction)
  val shadowPriest = Creature("ShadowOfPriest", Attack(3), 11, "Every turn heals 1 life to dark priest and all his creatures.", effects = effects(OnTurn -> shadowHeal))
  val heretic = Creature("Heretic", Attack(6), 20, "")
  val blackAngel = Creature("BlackAngel", Attack(8), 25, "When kills creature, completely heals itself", runAttack = new BlackAngelAttack)

  val DarkPriest : House = House("DarkPriest", List(
    Creature("Ghost", Attack(5), 16, "If killed with spell or creature ability, reborns and switches sides.\nWhen enters the game, heals to owner 1 life for each his creature on the board.", reaction = new GhostReaction, effects = effects(Direct -> ghostHeal)),
    Creature("Occultist", Attack(4), 20, "When enters the game, summons shadow of priest in opposite slot.", effects = effects(Direct -> occult)),
    Spell("BlackMass", "Sacrifices target creature and deals 4X damage to all enemy creatures\n(X - number of different elements to which enemy creatures belong).", inputSpec = Some(SelectOwnerCreature), effects = effects(Direct -> blackMass)),
    Creature("EnergyVampire", Attack(3), 23, "Every turn gives to owner 1 mana for each neighbour\n(element of mana = element of neighbour).", effects = effects(OnTurn -> evampire)),
    Creature("BlackMonk", Attack(4), 25, "When receives damage, heals the same amount of life to owner.", reaction = new BlackMonkReaction),
    Creature("Betrayer" , Attack(7), 38, "Can be summoned only on enemy creature which dies.\nEvery turn deals 4 damage to itself, to owner and neighbours.", inputSpec = Some(SelectTargetCreature), reaction = new BetrayerReaction, effects = effects(OnTurn -> betray)),
    Creature("DarkHydra", Attack(1), 32, "when attacks, damages opponent and all his creatures.\nAfter attack permanently increases its attack by 1 and heals X life to owner\n(X = attack power)", runAttack = new DarkHydraAttack),
    Creature("Missionary", Attack(3), 36, "When enters the game, weakest friendly creature and\nweakest enemy creature of the same element lose half of current health.\nWhen owner summons elemental creature, turns it into heretic\nWhen owner summons special creature, turns itself into black angel", effects = effects(Direct -> missionar), reaction = new MissionaryReaction)),
    effects = List(OnStart -> initRestless))

  val ghost = DarkPriest.cards(0).asCreature
  DarkPriest.initCards(Houses.basicCostFunc)

  def missionar = { env : Env =>
    import env._
    val weakest = player.slots.foldl(Option.empty[SlotUpdate]) { (acc, s) =>
      if (s.num == selected){
        acc
      } else {
        acc match {
          case None => Some(s)
          case Some(slot) =>
            if (slot.get.attack > s.get.attack){
              Some(s)
            } else acc
        }
      }
    }
    weakest.foreach{ w =>
      val houseIndex = w.get.card.houseIndex
      w.inflict(Damage(w.get.life / 2, isAbility = true))
      val weakestOther = otherPlayer.slots.foldl(Option.empty[SlotUpdate]) { (acc, s) =>
        if (s.get.card.houseIndex != houseIndex){
          acc
        } else {
          acc match {
            case None => Some(s)
            case Some(slot) =>
              if (slot.get.attack > s.get.attack){
                Some(s)
              } else acc
          }
        }
      }
      weakestOther.foreach{ s => s.inflict(Damage(s.get.life / 2, isAbility = true)) }
    }
  }
  def betray = { env : Env =>
    import env._
    val slot = env.player.slots(selected)
    val d = Damage(4, isAbility = true)
    focus()
    player.inflict(d)
    slot.inflict(d)
    slot.adjacentSlots.foreach(_.inflict(d))
  }
  def evampire = { env : Env =>
    import env._
    val adjacentHouses = player.slots(selected).adjacentSlots.flatMap(_.value).map(_.card.houseIndex)
    player.houses.incrMana(1, adjacentHouses : _*)
  }
  def blackMass = { env : Env =>
    import env._
    val slots = otherPlayer.slots.filleds
    val nbElems = slots.map{ s => s.get.card.houseId }.distinct.size
    otherPlayer.slots.inflictCreatures(Damage(4 * nbElems, isSpell = true), env.playerId)
    player.slots(selected).destroy()
  }
  def occult : Effect = { env : Env =>
    import env._
    val slot = otherPlayer.slots(selected)
    if (slot.value.isEmpty){
      slot.add(shadowPriest)
    }
  }
  def shadowHeal = { env : Env =>
    import env._
    focus()
    otherPlayer.heal(1)
    otherPlayer.slots.healCreatures(1)
  }
  def ghostHeal = { env : Env =>
    import env._
    val nbSlots = player.getSlots.size
    player.heal(nbSlots)
  }
  def initRestless = { env : Env =>
    env.otherPlayer.addEffect(OnEndTurn -> new SpawnRestless)
    spawnRestless(env.otherPlayer)
  }

  class SpawnRestless extends Function[Env, Unit]{
    def apply(env : Env){
      import env._
      if (!player.slots().exists{ case (n, slot) => slot.card == restlessSoul }){
        spawnRestless(player)
      }
    }
  }

  def spawnRestless(player : PlayerUpdate){
    val emptySlots = player.slots.getEmptySlots
    if (emptySlots.nonEmpty) {
      val slot = emptySlots(scala.util.Random.nextInt(emptySlots.size))
      slot.add(restlessSoul)
      slot.focus(blocking = false)
    }
  }

  class RestlessReaction extends Reaction {
    final override def onMyDeath(dead : Dead){
      dead.otherPlayer.houses.incrMana(2, 4)
    }
  }

  class GhostReaction extends Reaction {
    final override def onMyDeath(dead : Dead){
      if (dead.isEffect){
        val emptySlots = dead.otherPlayer.slots.getEmptySlots
        if (emptySlots.nonEmpty) {
          val slot = emptySlots(scala.util.Random.nextInt(emptySlots.size))
          slot.add(ghost)
          slot.focus(blocking = false)
        }
      }
    }
  }
  class MissionaryReaction extends Reaction {
    final override def onSummon(selected : Int, selectedPlayerId : PlayerId, summoned : SummonEvent) {
      import summoned._
      if (selectedPlayerId == player.id){
         if (card.houseIndex == 4){
          val slot = player.slots(selected)
          slot.destroy()
          slot.add(blackAngel)
        } else {
          val slot = player.slots(num)
          slot.destroy()
          slot.add(heretic)
        }
      }
    }
  }

  class BlackAngelAttack extends RunAttack {

    def apply(num : Int, d : Damage, player : PlayerUpdate) {
      val otherPlayer = player.otherPlayer
      val slot = otherPlayer.slots(num)
      if (slot.value.isEmpty) {
        otherPlayer.inflict(d, Some(SlotSource(player.id, num)))
      } else {
        slot.inflict(d)
        // FIXME maybe not good at all and should add source in damage?
        if (slot.value.isEmpty){
          player.slots(num).heal(blackAngel.life)
        }
      }
    }
  }
}

class BlackMonkReaction extends Reaction {
  final override def onProtect(selected : Int, d : DamageEvent) = {
    import d._
    target match { // hack
      case Some(n) if selected == n =>
        val life = player.getSlots(selected).life
        val healAmount = if (damage.amount > life) life else damage.amount
        player.heal(healAmount)
      case _ =>
    }
    damage
  }
}

class BetrayerReaction extends Reaction {
  final override def onSpawnOver(slot : SlotUpdate) {
    slot.destroy()
  }
}

class DarkHydraAttack extends RunAttack {
  def apply(num : Int, d : Damage, player : PlayerUpdate) {
    val otherPlayer = player.otherPlayer
    otherPlayer.inflict(d, Some(SlotSource(player.id, num)))
    otherPlayer.slots.inflictCreatures(d, player.id)
    player.heal(d.amount)
    val slot = player.slots(num)
    if (slot.value.isDefined){
      slot.attack.add(new OneAttackBonus) // TODO refactor
    }
  }
}
