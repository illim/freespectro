package priv.sp.update

import collection._
import priv.sp._
import priv.util.FieldUpdate
import CardSpec._

class PlayerUpdate(val id : PlayerId, val updater : GameStateUpdater) extends FieldUpdate(Some(updater), updater.state.players(id)) { playerFieldUpdate =>
  def pstate = value
  def updateListener = updater.updateListener
  def ended = updater.ended
  private var slotsUpdate = new SlotsUpdate(this)
  private var houseFieldUpdate = new HouseFieldUpdate

  def otherPlayer = updater.players(other(id)) // not great
  def getHouses   = if (houseFieldUpdate.isDirty) houseFieldUpdate.value else pstate.houses
  def getSlots    = if (slotsUpdate.isDirty) slotsUpdate() else pstate.slots // horrible perf probably
  def slots       = slotsUpdate.reinit()
  def houses      = houseFieldUpdate.reinit()
  // hack? for example avoid case of card moving during mass damage
  // not great on dead effect happens maybe too late
  def flush() = {
    if (slotsUpdate.isDirty){
      slotsUpdate.logs.reverseIterator.foreach{
        case dead : Dead => slots.onDead(dead)
        case _ =>
      }
      slots.logs = Nil
    }
  }
  def result = pstate.copy(slots = getSlots, houses = getHouses)

  def runSlots(){
    getSlots.foreach { case (num, _) =>
      getSlots.get(num).foreach{ slot =>
        if (slot.attack > 0 && slot.isRunnable && !ended){
          runSlot(num, slot)
        }
      }
    }
  }

  def runSlot(numSlot: Int, slot: SlotState) = {
    val d = Damage(slot.attack)
    slot.card.runAttack(numSlot, d, this)
    updateListener.runSlot(numSlot, id)
  }

  def submit(c : Command){
    val (test, newComand) = ((false, Option.empty[Command]) /: getSlots){ case (acc, (_, s)) =>
      if (acc._1) acc else s.card.reaction.interceptSubmit(c, updater)
    }
    (if (!test) Some(c) else newComand).foreach{ command =>
      if (command.card.isSpell){
        updateListener.spellPlayed(command)
      }
      houses.incrMana(- command.cost, command.card.houseIndex)
      updateListener.refresh(silent = true)
      if (!command.card.isSpell) {
        command.input.foreach{ slotInput =>
          val targetSlots = command.card.inputSpec match {
            case Some(SelectTargetCreature) =>
              otherPlayer.slots
            case _ =>
              slots
          }
          targetSlots.summon(slotInput.num, command.card.asCreature)
        }
      }
      command.card.effects(CardSpec.Direct) foreach { f =>
        val env = new GameCardEffect.Env(command.player, updater)
        command.input foreach { slotInput =>
          env.selected = slotInput.num
          if (command.card.inputSpec == Some(SelectOwnerSlot)){
            env.source = Some(SlotSource(command.player, slotInput.num))
          }
        }
        f(env)
      }
    }
  }

  def inflict(d : Damage, source : Option[SlotSource] = None) = {
    if (!ended) {
      val life = pstate.life - guard(mod(d), source).amount
      if (life <= 0){
        updater.ended = true
      }
      write(pstate.copy(life = life))
    }
  }

  def heal(amount : Int) {
    if (!ended) {
      write(pstate.copy(life = pstate.life + amount))
    }
  }

  def addEffect(effect : CardSpec.PhaseEffect){
    write(pstate.copy(effects = effect :: pstate.effects))
  }

  def removeEffect(cond : CardSpec.Effect => Boolean){
    write(pstate.copy(effects = pstate.effects.filter(e => !cond(e._2))))
  }

  // this is probably bugged due to card moves ...
  // todo identify slot creature?
  def applyEffects(phase : CardSpec.Phase) {
    getSlots.foreach{ case (num, slot) =>
      if (!ended) {
        getSlots.get(num).foreach{ slot => // looks weird because slots can change at each iteration
          slot.card.effects(phase).map{ f =>
            val env = new GameCardEffect.Env(id, updater)
            env.source = Some(SlotSource(id, num))
            env.selected = num
            f(env)
          }
          updateListener.refresh(silent = true)
        }
      }
    }
    pstate.effects.foreach{ case (p, f) =>
      if (p == phase && ! ended) {
        val env = new GameCardEffect.Env(id, updater)
        f(env)
      }
    }
  }

  def prepareNextTurn(){
    houses.incrMana()
  }

  def mod(d : Damage, playerId : PlayerId) : Damage = {
    if (playerId == id) otherPlayer.mod(d) else mod(d)
  }

  def mod(d : Damage) : Damage = {
    if (d.isSpell) {
      (d /: otherPlayer.getSlots){ case (acc, (_, slot)) =>
        slot.card.mod match {
          case Some(SpellMod(mod)) => acc.copy(amount = mod(acc.amount))
          case _ => acc
        }
      }
    } else d
  }

  def addDescMod(dmods : DescMod*)   { write(pstate.copy(desc = pstate.desc.add(dmods : _*))) }
  def removeDescMod(dmod : DescMod){ write(pstate.copy(desc = pstate.desc.remove(dmod))) }

  private def guard(damage : Damage, source : Option[SlotSource] = None) = {
    (damage /: getSlots) { case (acc, (num, slot)) =>
      val a = slot.card.mod match {
        case Some(SpellProtectOwner(mod)) => acc.copy(amount = mod(acc.amount))
        case _ => acc
      }
      slot.card.reaction.onProtect(num, DamageEvent(a, None, this, source))
    }
  }

  class HouseFieldUpdate extends FieldUpdate(Some(playerFieldUpdate), pstate.houses) {
    def houses = value

    def incrMana(incr : Int = 1){
      write(houses.map{ house =>
        val newmana = house.mana + incr
        new HouseState(math.max(0, newmana))
      })
      updateElementals()
    }

    def incrMana(amount : Int, houseIndex : Int*) {
      write(houseIndex.foldLeft(houses){ (acc, id) =>
        val house = acc(id)
        acc.updated(id, new HouseState(math.max(0, house.mana + amount)))
      })
      updateElementals()
    }

    def updateElementals(){
      if (getSlots.exists(_._2.card.attack.base.isEmpty)){
        slots.foreach { slot =>
          if (slot.attack.value.base.isEmpty){
            slot.attack.setDirty() // force reeval
          }
        }
      }
    }
  }
}

