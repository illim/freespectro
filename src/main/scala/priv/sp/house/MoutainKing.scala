package priv.sp.house

import priv.sp._
import priv.sp.update._
import CardSpec._
import GameCardEffect._

class MoutainKing {

  val soldier = Creature("Dwarven soldier", Attack(3), 12, "On entering the game increases attack of neighbors by 3 for 1 turn.\nHird: decreases attack of opposite creature by 1 for each\ndwarven soldier on the board.", effects = effects(Direct -> soldierEffect), reaction = new SoldierReaction)
  val shieldman = Creature("Dwarven shieldman", Attack(3), 15, "Redirects to himself half of the damage dealt to neighbors.\nHird: cannot be killed with magic (at least 1 hp will be left).", reaction = new ShieldmanReaction)
  val runesmith = Creature("Runesmith", AttackSources(Some(7), Vector(RuneAttackSource)), 23, "Runesmith and opposite creature can be harmed only by\neach other's attacks.\nHird: +1 attack for each dwarf in the game (including himself).", reaction = new RuneReaction)
  val ballista = Creature("Ballista", Attack(6), 34, "When enemy creature enters the game, halves its health and\nloses 10 health itself.\nHird: loses only 7 health on activating ability.", reaction = new BallistaReaction)
  val berserker = Creature("Berserker", AttackSources(Some(6), Vector(BerserkerAttackSource)), 40, "When owner receives more than 5 damage attacks out-of-turn opposite slot.\nHird: +3 attack and damage dealt to berserker.", reaction = new BerserkerReaction)
  val moutainKing = Creature("Moutain king", Attack(5), 45, "When allied dwarf enters the game, heals himself by 6 and\npermanently increases his attack by 2.\nWhen enters the game stuns strongest opponent creature.\nHird: reduces cost of dwarven cards by 1.", effects = effects(Direct -> moutain), reaction = new MountainReaction)

  val MoutainKing = House("Moutain King", List(
    soldier,
    shieldman,
    Creature("Dwarven crossbowman", Attack(4), 17, "When attacks deals the same damage directly to opponent.\nHird: attacks out-of-turn right before death.", runAttack = new CrossbowAttack, reaction = new CrossbowReaction),
    Creature("Armour-clad Dwarf", Attack(5), 23, "Reduces non-magical damage dealt to him by X\n(X = difference in level with opposite creature).\nHird: his ability applies to neighbors as well.", reaction = new ArmourReaction),
    runesmith,
    ballista,
    berserker,
    moutainKing),
    eventListener = Some(new CustomListener(new MKEventListener)))

  MoutainKing.initCards(Houses.basicCostFunc)


  def soldierEffect = { env : Env =>
    import env._
    val bonus = AttackAdd(3)
    getSelectedSlot().filledAdjacents.foreach(_.attack.add(bonus))
    player.addEffect(OnEndTurn -> new CountDown(1, { e =>
      e.player.slots(selected).filledAdjacents.foreach(_.attack.removeFirst(bonus))
    }))
  }

  def moutain = { env : Env =>
    import env._
    otherPlayer.slots.reduce(strongest _).foreach{ s =>
      s.toggle(CardSpec.stunFlag)
    }
  }

  class SoldierReaction extends Reaction {
    override def onMyRemove(slot : SlotUpdate) {
      val otherPlayer = slot.slots.player.otherPlayer
      otherPlayer.getSlots.get(slot.num) match {
        case Some(s) if s.attackSources.sources.contains(SoldierLowerAttack) =>
          otherPlayer.slots(slot.num).attack.removeFirst(SoldierLowerAttack)
        case _ =>
      }
      setSoldierOppAttackDirty(otherPlayer)
    }

    def setHird(s : SlotUpdate, b : Boolean, otherPlayer : PlayerUpdate){
      val oppSlot = otherPlayer.slots(s.num)
      if (oppSlot.value.isDefined){
        if (b) {
          if (!oppSlot.attack.has[SoldierLowerAttack.type]){
            oppSlot.attack.add(SoldierLowerAttack)
          }
        }
      }
      setSoldierOppAttackDirty(otherPlayer)
    }

    def setSoldierOppAttackDirty(otherPlayer : PlayerUpdate){
      otherPlayer.slots.foreach{ s =>
        if (s.attack.has[SoldierLowerAttack.type]){
          s.attack.setDirty()
        }
      }
    }
  }

  class RuneReaction extends Reaction {
    final override def selfProtect(d : Damage, slot : SlotUpdate) = {
      if (d.context.selected != slot.num || ! d.context.card.exists(!_.isSpell)) {
        d.copy(amount = 0)
      } else d
    }
  }

  case object RuneAttackSource extends AttackSlotStateFunc {
    def apply(attack : Int, slot : SlotUpdate) = {
      if (slot.value.exists(_.data == Hird)){
        val nbDwarf = slot.slots.filleds.count(_.get.card.houseId == MoutainKing.houseId)
        attack + nbDwarf
      } else attack
    }
  }

  case object SoldierLowerAttack extends AttackSlotStateFunc {
    def apply(attack : Int, slot : SlotUpdate) = {
      val oppSlots = slot.slots.player.otherPlayer.getSlots
      oppSlots.get(slot.num) match {
        case Some(s) if s.card == soldier && s.data == Hird =>
          val nbSoldier = oppSlots.count(_._2.card == soldier)
          math.max(0, attack - nbSoldier)
        case _ => attack
      }
    }
  }

  class ShieldmanReaction extends Reaction {
    lazy val someShieldman = Some(shieldman)

    final override def onProtect(selected : SlotUpdate, d : DamageEvent) = {
      import d._
      target match {
        case Some(num)
          if math.abs(num - selected.num) == 1
          && damage.context.card != someShieldman =>
            val context = Context(player.id, someShieldman, selected.num)
          val amount = d.damage.amount
          val samount = math.ceil(amount / 2.0).intValue
          selected.inflict(damage.copy(amount = samount, context= context))
          d.damage.copy(amount = amount - samount)
        case _ => d.damage
      }
    }

    override def selfProtect(d : Damage, slot : SlotUpdate) = {
      val life = slot.get.life
      if (slot.get.data == Hird && d.isEffect && d.amount >= life){
        d.copy(amount = life - 1)
      } else d
    }
  }

  class BallistaReaction extends Reaction {
    final override def onSummon(selected : Int, selectedPlayerId : PlayerId, summoned : SummonEvent) {
      import summoned._
      if (selectedPlayerId != player.id){
        val context = Context(selectedPlayerId, Some(ballista), selected)
        val damage = Damage(math.ceil(player.slots(num).get.life / 2.0).intValue, context, isAbility = true)
        player.updater.focus(selected, selectedPlayerId)
        player.slots(num).inflict(damage)
        val balSlot = player.otherPlayer.slots(selected)
        balSlot.inflict(Damage(if (balSlot.get.data == Hird) 7 else 10, context, isAbility = true))
      }
    }
  }

  class BerserkerReaction extends Reaction {
    // bs if iceguard is on the right of berserker
    def onPlayerDamage(amount : Int, slot : SlotUpdate) = {
      if (amount > 5){
        slot.slots.player.runSlot(slot.num, slot.get)
      }
    }
    override def selfProtect(d : Damage, slot : SlotUpdate) = {
      if (slot.get.data == Hird){
        d.copy(amount = d.amount + 3)
      } else d
    }
  }

  case object BerserkerAttackSource extends AttackSlotStateFunc {
    def apply(attack : Int, slot : SlotUpdate) = {
      if (slot.value.exists(_.data == Hird)){
        attack + 3
      } else attack
    }
  }

  class MountainReaction extends Reaction {
    final override def onMyRemove(slot : SlotUpdate) {
      if (slot.get.data == Hird){
        setHird(false, slot.slots.player)
      }
    }
    final override def onSummon(selected : Int, selectedPlayerId : PlayerId, summoned : SummonEvent) {
      import summoned._
      if (selectedPlayerId == player.id && card.houseId == MoutainKing.houseId){
        val slot = player.slots(selected)
        slot.heal(6)
        slot.attack.add(AttackAdd(2))
      }
    }
    def setHird(b : Boolean, player : PlayerUpdate){
      if (b) player.addDescMod(LowerSpecialCostMod)
      else player.removeDescMod(LowerSpecialCostMod)
    }
  }

  // crap
  class MKEventListener extends HouseEventListener {
    val moutainHouseId = MoutainKing.houseId
    override def protect(slot : SlotUpdate, damage : Damage) = {
      player.slots.foldl(damage) { (acc, s) =>
        val sc = s.get.card
        if (sc.houseIndex == 4){
          s.get.card.reaction.onProtect(s, DamageEvent(acc, Some(slot.num), player))
        } else acc
      }
    }

    override def protectOpp(slot : SlotUpdate, damage : Damage) = {
      player.getSlots.get(slot.num) match {
        case Some(s) if s.card == runesmith => s.card.reaction.selfProtect(damage, slot)
        case _ => damage
      }
    }
    override def onDeath(dead : Dead) {
      val c = dead.card
      if (dead.player.id == player.id && c.houseId == moutainHouseId) {
        player.slots.foreach{ s =>
          if (s.num != dead.num) {
            val c = s.get.card
            if (c.houseId == moutainHouseId) {
              if (math.abs(s.num - dead.num) == 1){
                val sym = s.num + (s.num - dead.num)
                if (!inSlotRange(sym)
                    || !player.slots(sym).value.exists(_.card.houseId == moutainHouseId)) {
                      setHird(s, false)
                    }
              }
              c.reaction.onDeath(s.num, player.id, dead)
            }
          }
        }
      }
    }
    override def onAdd(slot : SlotUpdate){
      if (slot.playerId == player.id){
        if (slot.get.card.houseId == moutainHouseId){
          if (slot.filledAdjacents.count{ s =>
            if (s.get.card.houseId == moutainHouseId){
              if (s.get.data == null){
                setHird(s, true)
              }
              true
            } else false
          } > 0){
            setHird(slot, true)
          }
        }
      } else {
        player.getSlots.get(slot.num) match {
          case Some(s) if s.card == soldier && s.data == Hird =>
            slot.attack.add(SoldierLowerAttack)
          case _ =>
        }
      }
    }
    override def onPlayerDamage(amount : Int){
      player.slots.foreach{ s =>
        val c = s.get.card
        if (c.houseIndex == 4){
          c.reaction match {
            case br : BerserkerReaction => br.onPlayerDamage(amount, s)
            case _ =>
          }
        }
      }
    }

    private def setHird(s : SlotUpdate, b : Boolean){
      val c = s.get.card
      s.setData(if (b) Hird else null)
      c.reaction match {
        case sr : SoldierReaction => sr.setHird(s, b, player.otherPlayer)
        case mr : MountainReaction => mr.setHird(b, player)
        case _ =>
      }
      if (c == runesmith || c == berserker){
        s.attack.setDirty()
      }
    }

  }
}


case object LowerSpecialCostMod extends DescMod {
  def apply(house : House, cards : Vector[CardDesc]) : Vector[CardDesc] = {
    if (house.houseIndex != 4) cards
    else cards.map(c => c.copy( cost = math.max(0, c.cost - 1)))
  }
}

class CrossbowAttack extends RunAttack {

  def apply(num : Int, d : Damage, player : PlayerUpdate) {
    val otherPlayer = player.otherPlayer
    val slot = otherPlayer.slots(num)
    otherPlayer.inflict(d)
    if (slot.value.isDefined) {
      slot.inflict(d)
    }
  }
}

class CrossbowReaction extends Reaction {
  // HACK should be on my death but the slot would already be empty
  final override def onMyRemove(slot : SlotUpdate){
    if (slot.get.data == Hird){
      slot.setData(null)
      slot.slots.player.runSlot(slot.num, slot.get)
    }
  }
}

class ArmourReaction extends Reaction {
  final override def selfProtect(d : Damage, slot : SlotUpdate) = protectFromOpp(d, slot)

  final override def onProtect(selected : SlotUpdate, d : DamageEvent) = {
    import d._
    d.target match {
      case Some(num) if selected.get.data == Hird
        && math.abs(selected.num - num) == 1 =>
          protectFromOpp(damage, player.slots(num))
      case _ => d.damage
    }
  }

  def protectFromOpp(d : Damage, slot : SlotUpdate) = {
    val oppSlot = slot.slots.player.otherPlayer.getSlots.get(slot.num)
    val levelDiff = oppSlot.map(s => math.max(0, s.card.cost - slot.get.card.cost)) getOrElse 0
    if (!d.isEffect && levelDiff != 0){
      d.copy(amount = math.max(0, d.amount - levelDiff))
    } else d
  }
}

case object Hird
