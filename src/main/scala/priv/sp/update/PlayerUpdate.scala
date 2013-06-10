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
  val houseEventListener = updater.desc.players(id).houses(4).house.eventListener.map(_()).getOrElse(new HouseEventListener)
  houseEventListener.player = this
  val stats = PlayerStats()
  protected lazy val otherPlayerStats = updater.playerFieldUpdates(other(id)).stats

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
        case dead : Dead =>
          stats.nbDead += 1
          otherPlayerStats.addKill(dead.card)
          slots.onDead(dead)
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
    val d = Damage(slot.attack, Context(id, Some(slot.card), numSlot))
    slot.card.runAttack(numSlot, d, this)
    updateListener.runSlot(numSlot, id)
  }

  def submit(c : Command){
    val (test, newComand) = houseEventListener.interceptSubmit(c)
    (if (!test) Some(c) else newComand).foreach{ command =>
      if (command.card.isSpell){
        updateListener.spellPlayed(command)
      }
      houses.incrMana(- command.cost, command.card.houseIndex) // could be after but it's ugly(-> hack for fury)
      updateListener.refresh(silent = true)
      if (!command.card.isSpell) {
        command.input.foreach{ slotInput =>
          val targetSlots = command.card.inputSpec match {
            case Some(SelectTargetCreature) =>
              otherPlayer.slots
            case _ =>
              slots
          }
          stats.nbSummon += 1
          targetSlots.summon(slotInput.num, command.card.asCreature)
        }
      }
      command.card.effects(CardSpec.Direct) foreach { f =>
        val env = new GameCardEffect.Env(command.player, updater)
        env.card = Some(command.card)
        command.input foreach { slotInput =>
          env.selected = slotInput.num
        }
        f(env)
      }
      updateListener.refresh()
    }
  }

  def inflict(d : Damage) = {
    if (!ended) {
      val life = pstate.life - guard(mod(d)).amount
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

  def mapEffect(f : CardSpec.Effect => CardSpec.Effect){
    write(pstate.copy(effects = pstate.effects.map( x => (x._1, f(x._2)))))
  }

  def addTransition(t : Transition){
    write(pstate.copy(transitions = t :: pstate.transitions))
  }

  def popTransition : Option[Transition] = {
    pstate.transitions match {
      case Nil => None
      case head :: tail =>
        write(pstate.copy(transitions = tail))
        Some(head)
    }
  }

  def blockSlot(n : Int){
    write(pstate.copy(slotList = pstate.slotList.filterNot(_ == n)))
  }

  // this is probably bugged due to card moves ...
  // todo identify slot creature?
  def applyEffects(phase : CardSpec.Phase) {
    getSlots.foreach{ case (num, slot) =>
      if (!ended) {
        getSlots.get(num).foreach{ slot => // looks weird because slots can change at each iteration
          if (!slot.has(CardSpec.blockedFlag)){
            slot.card.effects(phase).map{ f =>
              val env = new GameCardEffect.Env(id, updater)
              env.selected = num
              env.card = Some(slot.card)
              f(env)
            }
            updateListener.refresh(silent = true)
          }
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

  def mod(d : Damage) : Damage = {
    if (d.context.playerId == id) otherPlayer.mod(d)
    else {
      if (d.isSpell) {
        (d /: otherPlayer.getSlots){ case (acc, (_, slot)) =>
          slot.card.mod match {
            case Some(SpellMod(mod)) => acc.copy(amount = mod(acc.amount))
            case _ => acc
          }
        }
      } else d
    }
  }

  def addDescMod(dmods : DescMod*)   { write(pstate.copy(desc = pstate.desc.add(dmods : _*))) }
  def removeDescMod(dmod : DescMod){ write(pstate.copy(desc = pstate.desc.remove(dmod))) }

  private def guard(damage : Damage) = {
    (damage /: getSlots) { case (acc, (num, slot)) =>
      val a = slot.card.mod match {
        case Some(SpellProtectOwner(mod)) => acc.copy(amount = mod(acc.amount))
        case _ => acc
      }
      slot.card.reaction.onProtect(num, DamageEvent(a, None, this))
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

