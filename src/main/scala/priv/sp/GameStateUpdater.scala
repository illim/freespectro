package priv.sp

import collection._
import scalaz._
import priv.util.FieldUpdate

// sandbox horror, not thread safe (reusable static structure decomposition to update fields)
// should be more flexible than using lens
class GameStateUpdater(initState : GameState) extends FieldUpdate(None, initState) { self =>
  private var ended = false
  private val playerFieldUpdates = playerIds.map(id => new PlayerFieldUpdate(id))
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

  def players(id : PlayerId) = playerFieldUpdates(id).reinit()

  class PlayerFieldUpdate(id : PlayerId) extends FieldUpdate(Some(self), state.players(id)) { playerFieldUpdate =>
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


    def inflict(d : Damage) = {
      if (!ended) {
        val life = pstate.life - guard(mod(d).amount)
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

    def mod(d : Damage) = {
      if (d.isSpell) {
        (d /: otherPlayer.getSlots){ case (acc, (_, slot)) =>
          slot.card.mod match {
            case Some(SpellMod(mod)) => acc.copy(amount = mod(acc.amount))
            case _ => acc
          }
        }
      } else d
    }

    def guard(amount : Int) = {
      (amount /: getSlots) { case (acc, (num, slot)) =>
        val a = slot.card.mod match {
          case Some(SpellProtectOwner(mod)) => mod(acc)
          case _ => acc
        }
        slot.card.reaction.onProtect(num, DamageEvent(a, None, id, self))
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
        slots.get(num) foreach { slot =>
          damageSlot(mod(damage), num, slot)
        }
      }

      def inflictCreatures(damage : Damage) {
        val d = mod(damage)
        slots.foreach { case (num, slot) =>
          damageSlot(d, num, slot)
        }
      }

      def inflictMultiTarget(damage : Damage) {
        inflict(damage)
        inflictCreatures(damage)
      }

      def healCreature(num : Int, amount : Int) = {
        write(slots + (num -> SlotState.addLife(slots(num), amount)))
      }

      def protect(num : Int, amount : Int) = {
        (amount /: getSlots) { case (acc, (n, slot)) =>
          slot.card.reaction.onProtect(n, DamageEvent(acc, Some(num), id, self))
        }
      }

      def summon(num : Int, creature : Creature) {
        add(num, creature)
        otherPlayer.slots.reactSummon(num)
      }

      def add(num : Int, card : Creature){
        val slotState = applySlotEffects(num,
          SlotState(card, card.life, card.runOnce, card.attack getOrElse houses.value(card.houseIndex).mana, card.data))
        val newSlots = card.slotEffect.applySlots(num, slots + (num -> slotState))
        write(newSlots)
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
          val newSlot = applySlotEffects(num,
            SlotState(slot.card, slot.life, slot.hasRunOnce, slot.card.attack getOrElse houses.value(slot.card.houseIndex).mana, slot.data))
          write((slots - num) + (dest -> newSlot))
        }
      }

      private def damageSlot(damage : Damage, num : Int, slot : SlotState) = {
        val d = damage.copy(amount = protect(num, damage.amount))
        slot.inflict(d) match {
          case None          => destroy(num)
          case Some(newslot) => write(slots + (num -> newslot))
        }
      }

      def destroy(num : Int){
        val card = slots(num).card
        val newSlots = card.slotEffect.unapplySlots(num, slots)
        write(newSlots - num)
        val dead = Dead(num, card, id, self)
        logs = dead :: logs
      }

      def reactSummon(num : Int) = {
        slots.foreach{ case (_, slot) =>
          slot.card.boardEffect match {
            case Some(InterceptSpawn(d)) =>
              otherPlayer.slots.inflictCreature(num, d)
            case _ =>
          }
        }
      }
    }
  }

  def result = state.copy(players = playerIds.map{ id =>
    val fp = playerFieldUpdates(id)
    if (fp.isDirty) fp.result else state.players(id)
  })
}
