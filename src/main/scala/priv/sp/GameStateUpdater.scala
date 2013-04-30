package priv.sp

import collection._
import scalaz._
import priv.util.FieldUpdate

// sandbox horror, not thread safe (reusable static structure decomposition to update fields)
// should be more flexible than using lens
class GameStateUpdater(initState : GameState) extends FieldUpdate(None, initState) { self =>
  private val playerFieldUpdates = playerIds.map(id => new PlayerFieldUpdate(id))
  var ended = false
  var updateListener : UpdateListener = new DefaultUpdateListener

  def state = value

  def apply(value : GameState) = {
    ended = false
    initNewUpdate(value)
  }

  def lift[R](f : GameStateUpdater => R) : State[GameState, R] = State{ st : GameState =>
    val update = apply(st)
    val res = f(update)
    (update.result, res)
  }

  def focus(num : Int, playerId : PlayerId, blocking : Boolean = true) = updateListener.focus(num, playerId, blocking)

  def players(id : PlayerId) = playerFieldUpdates(id).reinit()

  class PlayerFieldUpdate(val id : PlayerId) extends FieldUpdate(Some(self), state.players(id)) { playerFieldUpdate =>
    def pstate = value
    private var slotFieldUpdate = new SlotFieldUpdate
    private var houseFieldUpdate = new HouseFieldUpdate

    def otherPlayer = players(other(id)) // not great
    def getHouses   = if (houseFieldUpdate.isDirty) houseFieldUpdate.value else pstate.houses
    def getSlots    = if (slotFieldUpdate.isDirty)  slotFieldUpdate.value  else pstate.slots
    def slots       = slotFieldUpdate.reinit()
    def houses      = houseFieldUpdate.reinit()
    def result = {
      if (slotFieldUpdate.isDirty){
        // dumb hack (should be a mutable queue) to avoid case of card moving during mass damage
        slotFieldUpdate.logs.foreach{
          case dead : Dead =>
            slots.update(s => dead.card.slotEffect.unapplySlots(dead.num, s))
            dead.card.reaction.onDeath(dead.num, dead)
            slots.value.foreach{ case (n, slot) =>
              if (n != dead.num) slot.card.reaction.onDeath(n, dead)
            }
          case _ =>
        }
        slots.logs = Nil
      }
      pstate.copy(slots = getSlots, houses = getHouses)
    }

    def runSlots(){
      getSlots.toList.sortBy(_._1).foreach { case (num, _) =>
        getSlots.get(num).foreach{ slot =>
          if (slot.attack > 0 && slot.hasRunOnce){
            runSlot(num, slot)
          }
        }
      }
    }

    def runSlot(numSlot: Int, slot: SlotState) = {
      val d = Damage(slot.attack)
      slot.card.runAttack(numSlot, d, self, id)
      updateListener.runSlot(numSlot, id)
    }

    def submit(c : Command){
      val (test, newComand) = ((false, Option.empty[Command]) /: getSlots){ case (acc, (_, s)) =>
        if (acc._1) acc else s.card.reaction.interceptSubmit(c, self)
      }
      (if (!test) Some(c) else newComand).foreach{ command =>
        if (command.card.isSpell){
          updateListener.spellPlayed(command)
        }
        houses.incrMana(- command.cost, command.card.houseIndex)
        updateListener.refresh(silent = true)
        if (!command.card.isSpell) {
          command.input.foreach{ slotInput =>
            slots.summon(slotInput.num, command.card.asCreature)
          }
        }
        command.card.effects(CardSpec.Direct) foreach { f =>
          val env = new GameCardEffect.Env(command.player, self)
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
        val life = pstate.life - guard(mod(d).amount, source)
        if (life <= 0){
          ended = true
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
              val env = new GameCardEffect.Env(id, self)
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
          val env = new GameCardEffect.Env(id, self)
          f(env)
        }
      }
    }

    def prepareNextTurn(){
      slots.toggleRun()
      houses.incrMana()
    }

    private def mod(d : Damage) = {
      if (d.isSpell) {
        (d /: otherPlayer.getSlots){ case (acc, (_, slot)) =>
          slot.card.mod match {
            case Some(SpellMod(mod)) => acc.copy(amount = mod(acc.amount))
            case _ => acc
          }
        }
      } else d
    }

    private def guard(amount : Int, source : Option[SlotSource] = None) = {
      (amount /: getSlots) { case (acc, (num, slot)) =>
        val a = slot.card.mod match {
          case Some(SpellProtectOwner(mod)) => mod(acc)
          case _ => acc
        }
        slot.card.reaction.onProtect(num, DamageEvent(a, None, id, self, source))
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
        if (getSlots.exists(_._2.card.attack.isEmpty)){
          slots.update{ s =>
            s.map{ case (num, slot) =>
              num -> (if (slot.card.attack.isEmpty) slots.applySlotEffects(num, slot.copy(attack = houses(slot.card.houseIndex).mana)) else slot)
            }
          }
        }
      }
    }

    class SlotFieldUpdate extends FieldUpdate(Some(playerFieldUpdate), pstate.slots){
      var logs = List.empty[BoardEvent]

      def slots = value

      def toggleRun()= write(slots.map{ case (i, slot) => i -> slot.copy( hasRunOnce = true) })

      def inflictCreature(num : Int, damage : Damage){
        damageSlot(mod(damage), num)
      }

      def inflictCreatures(damage : Damage) {
        val d = mod(damage)
        slots.toList.sortBy(_._1).foreach { case (num, _) =>
          damageSlot(d, num)
        }
      }

      def healCreature(num : Int, amount : Int) = {
        write(slots + (num -> SlotState.addLife(slots(num), amount)))
      }

      def protect(num : Int, amount : Int) = {
        (amount /: getSlots) { case (acc, (n, slot)) =>
          slot.card.reaction.onProtect(n, DamageEvent(acc, Some(num), id, self, None))
        }
      }

      def summon(num : Int, creature : Creature) {
        val slot = add(num, creature)
        updateListener.summon(num, slot, id)
        val summonEvent = SummonEvent(num, creature, id, self)
        otherPlayer.slots.reactSummon(summonEvent)
        reactSummon(summonEvent)
      }

      def add(num : Int, card : Creature) = {
        val slotState = applySlotEffects(num,
          SlotState(card, card.life, card.runOnce, card.attack getOrElse houses.value(card.houseIndex).mana, card.data))
        val newSlots = card.slotEffect.applySlots(num, slots + (num -> slotState))
        write(newSlots)
        slotState
      }

      // beware not mising effect
      def update(f : PlayerState.SlotsType => PlayerState.SlotsType) = write(f(slots))

      def setData(num : Int, data : AnyRef){
        val slot = slots(num)
        write(slots + (num -> slot.copy(data = data)))
      }

      def applySlotEffects(num : Int, slotState : SlotState) = {
        (slotState /: slots){ case (acc, (n, s)) =>
          s.card.slotEffect.applySlot(n, num, acc)
        }
      }

      def move(num : Int, dest : Int){
        slots.get(num).foreach{ slot =>
          // HACK (have to recreate the slot to recalcul attack)
          val newSlot = applySlotEffects(dest,
            SlotState(slot.card, slot.life, slot.hasRunOnce, slot.card.attack getOrElse houses.value(slot.card.houseIndex).mana, slot.data))
          updateListener.move(num, dest, id)
          write(
            slot.card.slotEffect.applySlots(dest, slot.card.slotEffect.unapplySlots(num, slots -num) + (dest -> newSlot)))
        }
      }

      private def damageSlot(damage : Damage, num : Int) = {
        if (slots.isDefinedAt(num)){
          val d = damage.copy(amount = protect(num, damage.amount)) // /!\ possible side effect
          slots(num).inflict(d) match {
            case None          => destroy(num)
            case Some(newslot) => write(slots + (num -> newslot))
          }
        }
      }

      def destroy(num : Int){
        val card = slots(num).card
        write(slots - num)
        val dead = Dead(num, card, id, self)
        logs = dead :: logs
      }

      def reactSummon(summonEvent : SummonEvent) = {
        slots.foreach{ case (num, slot) =>
          slot.card.reaction.onSummon(num, id, summonEvent)
        }
      }
    }
  }

  def result = state.copy(players = playerIds.map{ id =>
    val fp = playerFieldUpdates(id)
    if (fp.isDirty) fp.result else state.players(id)
  })
}


trait UpdateListener {
  def focus(num : Int, playerId : PlayerId, blocking : Boolean = true)
  def move(num : Int, dest : Int, playerId : PlayerId)
  def runSlot(num : Int, playerId : PlayerId)
  def summon(num : Int, slot : SlotState, playerId : PlayerId)
  def refresh(silent : Boolean = false) // this is not great to have some gui code here
  def spellPlayed(c : Command)
}

class DefaultUpdateListener extends UpdateListener {
  def focus(num : Int, playerId : PlayerId, blocking : Boolean){ }
  def move(num : Int, dest : Int, playerId : PlayerId) {}
  def runSlot(num : Int, playerId : PlayerId){}
  def summon(num : Int, slot : SlotState, playerId : PlayerId){}
  def refresh(silent : Boolean){}
  def spellPlayed(c : Command){}
}
