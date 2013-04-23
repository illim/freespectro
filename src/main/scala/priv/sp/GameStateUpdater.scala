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
    def result = pstate.copy(slots = getSlots, houses = getHouses)
    def getHouses = if (houseFieldUpdate.isDirty) houseFieldUpdate.value  else pstate.houses
    def getSlots = if (slotFieldUpdate.isDirty) slotFieldUpdate.value  else pstate.slots
    def slots = slotFieldUpdate.reinit()
    def houses = houseFieldUpdate.reinit()

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
        d.copy(amount = ((d.amount /: otherPlayer.getSlots){
          case (acc, (_, slot)) =>
            slot.card.mod match {
              case Some(SpellMod(mod)) => mod(acc)
              case _ => acc
            }
        }))
      } else d
    }

    def guard(amount : Int) = {
      (amount /: getSlots) {
        case (acc, (_, slot)) =>
          slot.card.mod match {
            case Some(SpellProtectOwner(mod)) => mod(acc)
            case _ => acc
          }
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
          val getBonus = slots.attackBonusGetter
          slots.update{ slots =>
            slots.map{ case (num, slot) =>
              num -> (if (slot.card.attack.isEmpty) slot.copy(attack = houses(slot.card.houseIndex).mana + getBonus(num)) else slot)
            }
          }
        }
      }
    }

    class SlotFieldUpdate extends FieldUpdate(Some(playerFieldUpdate), pstate.slots){
      def slots = value

      def toggleRun()= {
        write(slots.map{ case (i, slot) => i -> slot.copy( hasRunOnce = true) })
      }

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

      def summon(num : Int, creature : Creature) {
        add(num, creature)
        otherPlayer.slots.reactSummon(num)
      }

      def add(num : Int, creature : Creature){
        val attack = attackBonusGetter(num) + creature.attack.getOrElse(0)
        val runOnce = runOnceBonusGetter(num) || creature.runOnce
        creature.boardEffect.collect{ case bonus : AddAttack =>
          write(applyAttackBonus(num, bonus))
        }
        write(slots + (num -> SlotState(creature, creature.life, runOnce, attack)))
      }

      // beware not mising effect
      def update(f : PlayerState.SlotsType => PlayerState.SlotsType) = {
        write(f(slots))
      }

      private def damageSlot(damage : Damage, num : Int, slot : SlotState) = {
        slot.inflict(damage) match {
          case None          => destroy(num)
          case Some(newslot) => write(slots + (num -> newslot))
        }
      }

      def destroy(num : Int){
        if (slots.get(num).isEmpty){
          println(slots + "/" + num + "/" + self.value)
        }
        val creature = slots(num).card
        creature.boardEffect.collect{ case bonus : AddAttack =>
          write(applyAttackBonus(num, bonus, -1))
        }
        write(slots - num)
        reactDeath(num, creature)
      }

      def attackBonusGetter : Int => Int = {
        (Function.const[Int, Int](0) _ /: slots){ case (acc, (num, slot)) =>
          slot.card.boardEffect match {
            case Some(AddAttack(amount, around)) => { n : Int =>
              if (!around || math.abs(n - num) == 1) acc(n) + amount else acc(n)
            }
            case _ => acc
          }
        }
      }

      def runOnceBonusGetter : Int => Boolean = {
        (Function.const[Boolean, Int](false) _ /: slots){ case (acc, (num, slot)) =>
          slot.card.boardEffect match {
            case Some(ToggleRunAround) => { n : Int =>
              if (math.abs(n - num) == 1) true else acc(n)
            }
            case _ => acc
          }
        }
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

      def reactDeath(num : Int, creature : Creature) = {
        creature.boardEffect match {
          case Some(Reborn(cond)) if cond(pstate) =>
            summon(num, creature)
          case _ =>
        }
      }

      def applyAttackBonus(num : Int, bonus : AddAttack, fact : Int = 1) = {
        (slots /: slots){ case (acc, (n, slot)) =>
          acc + (n -> (if (!bonus.around || math.abs(n - num) == 1){
            slot.copy(attack = slot.attack + fact * bonus.amount)
          } else slot))
        }
      }
    }
  }

  def result = state.copy(players = playerIds.map{ id =>
    val fp = playerFieldUpdates(id)
    if (fp.isDirty) fp.result else state.players(id)
  })
}
