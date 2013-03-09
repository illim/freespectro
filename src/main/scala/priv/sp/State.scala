package priv.sp

import util.Random
import collection._

case class GameState(players: List[PlayerState])
case class PlayerState(
  houses: Vector[HouseState],
  slots: PlayerState.SlotsType = PlayerState.emptySlots,
  life: Int = 60) {

  var mods : List[Mod] = Nil // /!\ recomputed in lens
  var isSlotDependsMana = false

  def guard(amount : Int) = {
    (amount /: mods){
      case (acc, mod : SpellProtectOwner) => mod.modify(acc)
      case (acc, _) => acc
    }
  }

  def resetSlotAttack(){
    slots.values.foreach{ slot => slot.attack = slot.card.attack getOrElse houses(slot.card.houseIndex).mana }
  }

  val manaHash = (0 /: houses.zipWithIndex) { case (acc, (house, i)) =>
    acc + (house.mana * math.pow(10, i)).intValue
  }
}
class HouseState(val mana: Int) extends AnyVal
case class SlotState(card: Creature, life: Int, var attack: Int = 0, hasRunOnce: Boolean = false){
  def inflict(damage : Damage) : Option[SlotState] = {
    val newlife = card.inflict(damage, life)
    if (newlife < 1) None else Some(copy(life = newlife))
  }
  def addAttack(x : Int) = {
    if (card.attack.isEmpty || card.attack != Some(0)){
      attack += x
    }
  }
}

// Description (should not change during the game)
case class GameDesc(players : Array[PlayerDesc])
case class PlayerDesc(houses : Array[PlayerHouseDesc]){
  houses.zipWithIndex.foreach{ case (house, i) => house.index = i}
}
case class PlayerHouseDesc(house : House, cards : Array[Card]){
  var index = 0
  val cardList = cards.toList
}

import scalaz._
object PlayerState {
  type SlotsType = immutable.Map[Int, SlotState]
  val emptySlots = immutable.Map.empty[Int, SlotState]
  val housesL = Lens.lensu[PlayerState, Vector[HouseState]]((p, h) => p.copy(houses = h), _.houses)
  val slotsL = Lens.lensu[PlayerState, SlotsType]((p, s) => p.copy(slots = s), _.slots)
  val lifeL = Lens.lensu[PlayerState, Int]((p, l) => p.copy(life = l), _.life)
}
object HouseState{
  def incrMana(houses : Vector[HouseState], amount : Int, houseIndex : Int*) = {
    houseIndex.foldLeft(houses){ (acc, id) =>
      val house = acc(id)
      acc.updated(id, new HouseState(if (amount < 0) math.min(0, house.mana + amount) else house.mana + amount))
    }
  }
}
object SlotState {
  def creature(card: Card) = {
    card match {
      case creature: Creature =>
        SlotState(creature, creature.life)
      case _ => sys.error(card + " is not a creature")
    }
  }
  val lifeL = Lens.lensu[SlotState, Int]((p, l) => p.copy(life = l), _.life)

  @inline def inflictCreature(player: PlayerStateLenses, numSlot : Int, damage : Damage) : State[GameState, Unit] = {
    player.slots.%== { slots =>
      val slot = slots(numSlot)
      slot.inflict(damage) match {
        case None => slots - numSlot
        case Some(newSlot) => slots + (numSlot -> newSlot)
      }
    }
  }

  @inline def addLife(slot : SlotState, amount : Int) = {
    slot.copy(life = math.min(slot.card.life, slot.life + amount))
  }

  @inline def inflictCreatures(player: PlayerStateLenses, damage : Damage) : State[GameState, Unit] = {
    player.slots.%==( damageSlots(damage) _)
  }

  def damageSlots(damage : Damage)(slots: PlayerState.SlotsType) = {
    var result = PlayerState.emptySlots
    slots.foreach { case (num, slot) =>
      slot.inflict(damage) foreach { newslot =>
        result += (num -> newslot)
      }
    }
    result
  }
}
class PlayerStateLenses(val player : Lens[GameState, PlayerState]){
  val houses = player andThen PlayerState.housesL
  val slots  = player andThen PlayerState.slotsL
  val life   = player andThen PlayerState.lifeL
  def slotsToggleRun = slots.%==(_.map{ case (i, slot) => i -> slot.copy( hasRunOnce = true) })
  def housesIncrMana = houses.%== (_.map{ house =>
    val newmana = house.mana + 1
    new HouseState(math.max(0, newmana))
  })
  def house(id : Int) = player andThen Lens.lensu[PlayerState, HouseState]((p, x) => p.copy(houses = p.houses.updated(id, x)), _.houses(id))
}
object GameState {
  val playersL = Lens.lensu[GameState, List[PlayerState]]((p, x) => p.copy(players = x), _.players)

  def playerLens(id : Int) = new PlayerStateLenses(Lens.lensu[GameState, PlayerState]({ (state, p) =>
    val oldp = state.players(id)
    val newState = state.copy(players = state.players.updated(id, p))

    // /!\ suppose that no slot is replaced in one pass
    // mods should not be used before state update
    // recompute all when house mana increase or slots number modified
    if (oldp.slots.size == p.slots.size && (!oldp.isSlotDependsMana || oldp.manaHash == p.manaHash)) {
      p.mods = oldp.mods
      p.isSlotDependsMana = oldp.isSlotDependsMana
    } else {
      p.isSlotDependsMana = p.slots.values.exists(_.card.attack.isEmpty)
      p.resetSlotAttack()
      val mods = (List.empty[Mod] /: p.slots){ case (acc, (num, slot)) =>
        slot.card.mod match {
          case None => acc
          case Some(mod) =>
            mod match {
              case AddAttackMod(x, around) =>
                if (around){
                  p.slots.get(num - 1).foreach( s => s.addAttack(x)) // /!\ side effect
                  p.slots.get(num + 1).foreach( s => s.addAttack(x))
                } else {
                  p.slots.foreach(_._2.addAttack(x))
                }
              case _ =>
            }
            (mod :: acc)
        }
      }
      p.mods = mods
    }

    newState
  }, _.players(id)))

  val unit = State[GameState, Unit](gs => (gs, ()))
  def none[A] = State[GameState, Option[A]](gs => (gs, None))
  val currentVersion = new java.util.concurrent.atomic.AtomicInteger(0)
}
// ai hacks
object GameDesc {
  val playersL = Lens.lensu[GameDesc, Array[PlayerDesc]]((p, x) => p.copy(players = x), _.players)
  def playerLens(id : Int) = Lens.lensu[GameDesc, PlayerDesc]((p, x) => p.copy(players = p.players.updated(id, x)), _.players(id))
  val housesL = Lens.lensu[PlayerDesc, Array[PlayerHouseDesc]]((p, h) => p.copy(houses = h), _.houses)
  def replaceCards(newHouses: Array[PlayerHouseDesc]) = housesL.%== (_.zipWithIndex.map{case (house, i) => house.copy(cards = newHouses(i).cards) })
}
