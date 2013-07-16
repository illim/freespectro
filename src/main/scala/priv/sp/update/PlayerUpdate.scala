package priv.sp.update

import collection._
import priv.sp._
import priv.util.FieldUpdate
import CardSpec._

class PlayerUpdate(val id : PlayerId, val updater : GameStateUpdater) extends FieldUpdate(Some(updater), updater.state.players(id)) { playerFieldUpdate =>
  def pstate = value
  def updateListener = updater.updateListener
  def ended = updater.ended
  val slotsUpdate = new SlotsUpdate(this)
  val housesUpdate = new HousesUpdate
  val houseEventListener = updater.desc.players(id).houses(4).house.eventListener.map{
    case OpponentListener(f) =>
      updater.desc.players(other(id)).houses(4).house.eventListener.collect{
        case c : CustomListener => f(c())
      }.getOrElse(f(new HouseEventListener))
    case c : CustomListener => c()
  }.getOrElse(new HouseEventListener)

  val stats = PlayerStats()
  val otherId = other(id)
  protected lazy val otherPlayerStats = updater.playerFieldUpdates(otherId).stats
  def otherHouseEventListener = updater.houseEventListeners(otherId)

  def otherPlayer = updater.players(other(id)) // not great
  def getHouses   = if (housesUpdate.isDirty) housesUpdate.value else value.houses
  def getSlots    = if (slotsUpdate.isDirty) slotsUpdate() else value.slots // horrible perf probably
  def slots       = slotsUpdate.reinit()
  def houses      = housesUpdate.reinit()
  // hack? for example avoid case of card moving during mass damage
  // not great on dead effect happens maybe too late
  def flush() = {
    if (isDirty) {
      if (slotsUpdate.isDirty){
        slotsUpdate.logs.reverseIterator.foreach {
          case dead : Dead =>
            stats.nbDead += 1
            otherPlayerStats.addKill(dead.slot)
            slots.onDead(dead)
          case _ =>
        }
        slots.logs = Nil
      }
    }
  }
  def result = value.copy(slots = getSlots, houses = getHouses)

  def runSlots(){
    getSlots.foreach { case (num, _) =>
      getSlots.get(num).foreach{ slot =>
        runSlot(num, slot)
      }
    }
  }

  def runSlot(numSlot: Int, slot: SlotState) = {
    if (slot.attack > 0 && slot.isRunnable && !ended){
      val d = Damage(slot.attack, Context(id, Some(slot.card), numSlot))
      updateListener.runSlot(numSlot, id)
      slot.card.runAttack(slot.target, d, this)
      updateListener.refresh()
    }
  }

  /**
   * Note : direct effects are applieds after add and their callbacks which is different
   * from Spectromancer. Example : trooper damages a summoned creature
   * before being killed by his effect. (alternative is to create a phase beforeadd, afteradd)
   */
  def submit(c : Option[Command]){
    val (test, newComand) = houseEventListener.interceptSubmit(c)
    (if (!test) c else newComand).foreach(submitCommand)
  }

  val submitCommand = new priv.util.ObservableFunc1({ command : Command =>
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
  })

  def inflict(d : Damage) = {
    if (!ended) {
      val amount = guard(mod(d)).amount
      val life = value.life - amount
      if (life <= 0){
        updater.ended = true
      } else {
        houseEventListener.onPlayerDamage(amount)
      }
      write(value.copy(life = life))
    }
  }

  def heal(amount : Int) {
    if (!ended) {
      write(value.copy(life = value.life + amount))
    }
  }

  def addEffect(effect : CardSpec.PhaseEffect){
    write(value.copy(effects = effect :: value.effects))
  }

  def removeEffect(cond : CardSpec.Effect => Boolean){
    write(value.copy(effects = value.effects.filter(e => !cond(e._2))))
  }

  def mapEffect(f : CardSpec.Effect => CardSpec.Effect){
    write(value.copy(effects = value.effects.map( x => (x._1, f(x._2)))))
  }

  def setData(data : AnyRef)  { write(value.copy( data = data)) }
  def updateData[A<: AnyRef](f : A => A)  { setData(f(value.data.asInstanceOf[A])) }

  def addTransition(t : Transition){
    write(value.copy(transitions = t :: value.transitions))
  }

  def popTransition : Option[Transition] = {
    value.transitions match {
      case Nil => None
      case head :: tail =>
        write(value.copy(transitions = tail))
        Some(head)
    }
  }

  def blockSlot(n : Int){
    write(value.copy(slotList = value.slotList.filterNot(_ == n)))
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
    value.effects.foreach{ case (p, f) =>
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
        (updater.houseEventListeners(d.context.playerId).mod(d) /: otherPlayer.getSlots){ case (acc, (_, slot)) =>
          slot.card.mod match {
            case Some(SpellMod(mod)) => acc.copy(amount = mod(acc.amount))
            case _ => acc
          }
        }
      } else d
    }
  }

  def addDescMod(dmods : DescMod*)   { write(value.copy(desc = value.desc.add(dmods : _*))) }
  def removeDescMod(dmod : DescMod){ write(value.copy(desc = value.desc.remove(dmod))) }

  // sub optimal?
  private def guard(damage : Damage) = {
    slots.foldl(damage) { case (acc, slot) =>
      slot.get.card.reaction.onProtect(slot, DamageEvent(acc, None, this))
    }
  }

  class HousesUpdate extends FieldUpdate(Some(playerFieldUpdate), pstate.houses) {
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
      if (getSlots.exists(_._2.card.attack.base.isEmpty)){ // bs
        slots.foreach { slot =>
          if (slot.attack.value.base.isEmpty){
            slot.attack.setDirty() // force reeval
          }
        }
      }
    }
  }
}

