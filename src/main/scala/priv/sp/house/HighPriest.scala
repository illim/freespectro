package priv.sp.house

import priv.sp._
import priv.sp.update._
import priv.sp.gui._
import GameCardEffect._
import CardSpec._

/**
 * Introduced bullshit :
 * bennu -> doing something before dying = crap
 * serpent of ... ->
 *    crap spawning just after death
 *    doing something on opponent's turn
 *
 * localized bs:
 * eyes of wajet -> store nb card played (bugged for abilities)
 * simoom -> store nb turn ^^
 *
 *
 * bugs: wajet
 * spawned serpent if 2 ouros
 */
class HighPriest {

  val apis = Creature("Apis", Attack(4), 20, "Every turn gives to owner 1 special power and\n3 hp for each other apis on the board.", effects = effects(OnTurn -> apisEffect))
  val sphynx =  Creature("Sphinx", Attack(8), 24, "When dies, leaves puzzle 0/6.\nIf puzzle was destroyed by enemy creature, sphinx reborns with halved hp.\nIf puzzle was destroyed by enemy spell or ability,\nopponent loses 3 power of highest element.", reaction = new SphinxReaction)
  val puzzle = Creature("puzzle", Attack(0), 6, "If puzzle was destroyed by enemy creature, sphinx reborns with halved hp.\nIf puzzle was destroyed by enemy spell or ability,\nopponent loses 3 power of highest element.", reaction = new PuzzleReaction)
  val ouroboros = Creature("Ouroboros", Attack(6), 38, "At the beginning of owner's turn summons in nearest empty slot\nserpent of eternity.", effects = effects(OnTurn -> ouro))
  val serpent = Creature("serpent of eternity", Attack(2), 8, "At the end of opponent's turn serpent dies and heals X hp to owner and Ouroboros (X = its remaining hp).")
  val sunStone = Creature("sun stone", Attack(0), 22, "increases damage from owner spells by 2 and increases Ra's attack by 1 every turn", mod = Some(new SpellMod(x => x + 2)), effects = effects(OnTurn -> incrRaAttack))
  val guardianMummy = Creature("guardian mummy", Attack(4), 20)
  val dragonOfRa = Creature("Winged dragon of Ra", Attack(6), 45, "When enters the game, summons sun stone in nearest empty slot.", effects = effects(Direct -> ra))
  val babi = Creature("Babi", Attack(4), 18, "When opponent's power grows, deals the same damage to opposite creature.", effects = effects(Direct -> initBabi), reaction = new BabiReaction)
  val amit = Creature("Ammit", Attack(4), 18, "When any creature dies, deals to its owner damage equal to his power\nof that element and gives 1 special power to owner.", reaction = new AmitReaction)

  val hpSet = List[Card](
    Creature("Ancient crocodile", Attack(8), 15, "when attacks, skips next turn (digestion).", runAttack = new CrocodileAttack),
    Creature("Serpopard", Attack(4), 18, "When owner summons special creature, moves in nearest unblocked slot\nand doubles attack for 1 turn.", reaction = new SerpoReaction),
    Creature("Anubite", Attack(4), 18, "When kills creature, summon in nearest empty slot guarding mummy.", runAttack = new AnubiteAttack),
    babi,
    Spell("Curse of chaos", "Deals to target creature and its neighbors damage equal to their total attack.",
          inputSpec = Some(SelectTargetCreature),
          effects = effects(Direct -> curse)),
    Creature("Simooom", Attack(4), 18, "Reduces attack of all enemy creatures to 1.\nThey restore 3 attack per turn since next turn.", effects = effects(Direct -> simoom)),
    amit,
    Creature("Apep", Attack(5), 50, "Attacks all enemies.\nEvery turn decreases elemental powers of both players by 1.", effects = effects(OnTurn -> apep)))

  val HighPriest : House = House("High Priest", List(
    Creature("Sacred scarab", Attack(3), 15, "decreases non-magical damage received by it by 2X\nX = number of its neighbors.", reaction = new ScarabReaction),
    Creature("Sun priest", Attack(3), 16, "When attacks, deals to all enemy creatures damage equal to\nowner's lowest power.", runAttack = new SunPriestAttack),
    apis,
    Creature("Bennu", Attack(5), 21, "If killed by enemy card, attacks opposite slot with tripled attack\nbefore death.", reaction = new BennuReaction),
    Spell("Eye of wajet", "heals to owner and his creatures 1 hp for each revealed enemy card and\ndeals the same damage to all enemies.", effects = effects(Direct -> wajet)),
    sphynx,
    ouroboros,
    dragonOfRa),
    effects = List(OnTurn -> hpTurn, OnStart -> init),
    eventListener = Some(new CustomListener(new HPriestEventListener)),
    data = HPriestData())

  HighPriest.initCards(Houses.basicCostFunc)
  HighPriest.initCards(Houses.basicCostFunc, hpSet)

  case class HPriestData(revealeds : Set[Card] = Set.empty, numTurn : Int = 0)
  def getData(p : PlayerState) = p.data.asInstanceOf[HPriestData]

  def init = { env: Env =>  choosePath(env.player)  }

  def choosePath(player : PlayerUpdate){
    val houses = player.getHouses
    val fireearth = houses(0).mana + houses(3).mana
    val waterair = houses(1).mana + houses(2).mana
    val hasMod = player.value.desc.descMods.contains(PathSet)
    if (fireearth > waterair) {
      if (hasMod) player.removeDescMod(PathSet)
    } else if (!hasMod) player.addDescMod(PathSet)
  }

  def hpTurn = { env : Env =>
    import env._
    player.updateData[HPriestData](x => x.copy( numTurn = x.numTurn + 1))
  }

  case object PathSet extends DescMod {
    def apply(house : House, cards : Vector[CardDesc]) : Vector[CardDesc] = {
      if (house.houseIndex == 4) {
        cards.map{ c =>
          CardDesc(hpSet(c.card.cost - 1)) // bs index is cost -1
        }
      } else cards
    }
  }

  def wajet = { env : Env =>
    import env._
    val nbRevealeds = getData(player.value).revealeds.size
    player.slots.healCreatures(nbRevealeds)
    otherPlayer.slots.inflictCreatures(Damage(nbRevealeds, env, isSpell = true))
  }

  def curse = { env : Env =>
    import env._
    val slot = otherPlayer.slots(selected)
    val slots = slot :: slot.filledAdjacents
    val damage = Damage(slots.map(_.get.attack).sum, env, isSpell = true)
    slots.foreach(_.inflict(damage))
  }

  def apisEffect : Effect = { env : Env =>
    import env._
    player.houses.incrMana(1, 4)
    val nbApis = player.slots.foldl(0){ (acc, s) =>
      if (s.get.card == apis) (acc + 1) else 0
    }
    if (nbApis > 1){
      player.heal(3 * (nbApis - 1))
      getSelectedSlot().focus()
    }
  }

  def ouro : Effect = { env : Env =>
    import env._
    nearestEmptySlot(selected, player).foreach{ pos =>
      player.slots(pos).add(serpent)
    }
    if (! getOtherPlayerState.effects.exists(_._2.isInstanceOf[SerpentDie])){
      player.otherPlayer.addEffect(OnEndTurn -> new SerpentDie)
    }
  }

  def ra : Effect = { env : Env =>
    import env._
    nearestEmptySlot(selected, player).foreach{ pos =>
      player.slots(pos).add(sunStone)
    }
  }

  def simoom = { env : Env =>
    import env._
    val nTurn = getData(player.value).numTurn
    val bonus = SimAttackReduction(nTurn)
    var maxAttack = 0
    otherPlayer.slots.foreach{ s =>
      val att = s.get.attack
      if (att > 1){
        if (att > maxAttack){
          maxAttack = att
        }
        s.attack.add(bonus)
      }
    }
    player.addEffect(OnEndTurn -> SimoomRefresh(bonus, math.ceil(maxAttack / 3f).toInt))
  }

  def incrRaAttack = { env : Env =>
    import env._
    player.slots.findCard(dragonOfRa).foreach{ s =>
      s.attack.add(AttackAdd(1))
    }
  }

  def initBabi = { env : Env =>
    import env._

    val mana = otherPlayer.houses.value.map(_.mana).sum
    getSelectedSlot().setData(new Integer(mana))
  }

  def apep = { env : Env =>
    env.player.houses.incrMana(-1, 0, 1, 2, 3)
    env.otherPlayer.houses.incrMana(-1, 0, 1, 2, 3)
  }

  // BULLSHIT (leak) this is crap. things happening on opponent's turn should belongs to opponent
  class SerpentDie extends Function[Env, Unit] {
    def apply(env : Env){
      import env._
      val ouros = otherPlayer.getSlots.collect{ case (i, s) if s.card == ouroboros => i}
      otherPlayer.getSlots.foreach{ case (i, s) =>
        if (s.card == serpent){
          val slots = otherPlayer.slots
          val slot = slots(i)
          val amount = s.life
          otherPlayer.heal(amount)
          ouros.foreach{ o => slots(o).heal(amount) }
          slot.focus()
          slot.destroy()
        }
      }
    }
  }

  class SphinxReaction extends Reaction {
    final override def onMyDeath(dead : Dead){
      dead.player.slots(dead.num).add(puzzle)
    }
  }
  class PuzzleReaction extends Reaction {
    final override def onMyDeath(dead : Dead){
      import dead._
      damage match {
        case Some(d) if !d.isEffect =>
          val otherPlayer = player.otherPlayer
          val houseId = (otherPlayer.getHouses.foldLeft((0, 0, 0)){ case ((hidx, m, idx), h) =>
            if (h.mana > m) (idx, h.mana, idx+1) else (hidx, m, idx +1)
          })._1
          otherPlayer.houses.incrMana(-3, houseId)
        case _ =>
          val slot = player.slots(dead.num)
          val slotState = player.slots.buildSlotState(slot, card)
          slot.add(slotState.copy(life = slotState.life/2))
      }
    }
  }

  class AnubiteAttack extends RunAttack {
    def apply(target : Option[Int], d : Damage, player : PlayerUpdate) {
      if (SingleTargetAttack.attack(target, d, player)){
        nearestEmptySlot(d.context.selected, player).foreach{ pos =>
          player.updater.focus(d.context.selected, player.id)
          player.slots(pos).add(guardianMummy)
        }
      }
    }
  }

  class BabiReaction extends Reaction {
    def reactIncrMana(selected : SlotUpdate){
      val otherPlayer = selected.slots.player.otherPlayer
      val old = selected.get.data.asInstanceOf[Integer]
      val mana = otherPlayer.houses.value.map(_.mana).sum
      if (mana > old){
        val oppSlot = selected.oppositeSlot
        if (oppSlot.value.isDefined){
          oppSlot.inflict(Damage(mana - old, Context(selected.playerId, Some(babi), selected.num), isAbility = true))
          selected.focus()
        }
      }
      selected.setData(new Integer(mana))
    }
  }

  class AmitReaction extends Reaction {
    final override def onDeath(selected : Int, playerId : PlayerId, dead : Dead){
      val power = dead.player.getHouses(dead.card.houseIndex).mana
      dead.player.inflict(Damage(power, Context(playerId, Some(amit), selected), isAbility = true))
      dead.player.houses.incrMana(1, 4)
    }
  }

  case class SimAttackReduction(numTurn : Int) extends AttackStateFunc {
    def apply(attack : Int, player : PlayerUpdate) : Int = {
      val curr = getData(player.updater.value.players(other(player.id))).numTurn
      1 + math.min((curr - numTurn) * 3, attack -1)
    }
  }

  case class SimoomRefresh(bonus : SimAttackReduction, turnCount : Int) extends Function[Env, Unit] {
    val maxTurn = bonus.numTurn + turnCount
    def apply(env : Env){
      import env._
      if (getData(player.value).numTurn > maxTurn){
        player.otherPlayer.slots.foreach{ s =>
          s.attack.removeAny(bonus)
        }
        player.removeEffect(_ == this)
      } else {
        player.otherPlayer.slots.foreach{ s =>
          s.attack.setDirty()
        }
      }
    }
  }

  class HPriestEventListener extends HouseEventListener {
    def onOppSubmit(command : Command){
      val data = getData(player.value)
      // hack for warp
      if (data != null && !data.revealeds.contains(command.card)){
        player.updateData[HPriestData](_.copy(revealeds = data.revealeds + command.card))
      }
    }

    override def init(p : PlayerUpdate){
      super.init(p)
      p.houses.update.after{ _ =>
        choosePath(p)
      }
      p.otherPlayer.houses.update.after{ _ =>
        p.slots.foreach{ s =>
          s.get.card.reaction match {
            case br : BabiReaction => br.reactIncrMana(s)
            case _ =>
          }
        }
      }
      p.otherPlayer.submitCommand.after(onOppSubmit _)
    }
  }
}

class ScarabReaction extends Reaction {
  override def selfProtect(d : Damage, slot : SlotUpdate) = {
    if (d.isSpell) d else {
      val nbAdjs = slot.adjacentSlots.count(_.value.isDefined)
      if (nbAdjs != 0){
        d.copy(amount = math.max(0, d.amount - 2 * nbAdjs))
      } else d
    }
  }
}

class SunPriestAttack extends RunAttack {
  isMultiTarget = true

  def apply(target : Option[Int], d : Damage, player : PlayerUpdate) {
    SingleTargetAttack.apply(target, d, player)
    val massD = d.copy(amount = player.getHouses.minBy(_.mana).mana)
    player.otherPlayer.slots.inflictCreatures(massD)
  }
}


class CrocodileAttack extends RunAttack {

  def apply(target : Option[Int], d : Damage, player : PlayerUpdate) {
    SingleTargetAttack.apply(target, d, player)
    player.slots(d.context.selected).toggle(CardSpec.pausedFlag)
    player.addEffect(OnTurn -> new CountDown(2, { env : Env =>
      env.player.slots(d.context.selected).toggleOff(CardSpec.pausedFlag)
    }))
  }
}

// hack for zen guard
case object BennuDead
class BennuReaction extends Reaction {
  final override def onMyRemove(slot : SlotUpdate, dead : Option[Dead]){
    // Bullshit ?
    if (slot.get.data != BennuDead && !dead.exists(_.damage.exists(_.context.playerId == slot.playerId))) {
      slot.attack.add(AttackFactor(3f))
      slot.slots.player.runSlot(slot.num, slot.get)
      slot.setData(BennuDead)
    }
  }
}

class SerpoReaction extends Reaction {
  final override def onSummon(selected : Int, selectedPlayerId : PlayerId, summoned : SummonEvent) = {
    import summoned._
    if (selected != num && card.houseIndex == 4 && selectedPlayerId == player.id){
      nearestSlotOpposed(selected, player, opposed = false).foreach{ n =>
        val slots = player.slots
        slots.move(selected, n)
        val bonus = AttackFactor(2f)
        slots(n).attack.add(bonus)
        player.addEffect(OnEndTurn -> { env : Env =>
          val s = env.player.slots(n)
          if (s.value.isDefined){
            s.attack.removeFirst(bonus)
          }
        })
      }
    }
  }
}

