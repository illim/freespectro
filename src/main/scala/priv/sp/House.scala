package priv.sp

import house._
import priv.sp.update._
import java.io._
import collection._

object House {
  val currentId = new java.util.concurrent.atomic.AtomicInteger
}
// eventListener & data is only used for special houses
case class House(
    name: String,
    cards: List[Card],
    houseIndex: Int = 4,
    effects: List[CardSpec.PhaseEffect] = Nil,
    eventListener: Option[ListenerBuilder] = None,
    data: AnyRef = null) extends Externalizable {
  def this() = this(null, Nil)

  var houseId = House.currentId.incrementAndGet()

  def costs = cards.map(_.cost)

  def initCards(costFn: Int ⇒ Int, cs: List[Card] = cards) {
    cs.zipWithIndex.foreach {
      case (c, i) ⇒
        c.cost = costFn(i)
        c.houseIndex = houseIndex
        c.houseId = houseId
        c.cardIndex = i
    }
  }

  override def toString() = name
  override def hashCode(): Int = name.hashCode
  override def equals(o: Any) = {
    o match {
      case h: House ⇒ h.hashCode() == hashCode()
      case _        ⇒ false
    }
  }
  def writeExternal(out: ObjectOutput) { out.writeInt(houseId) }
  def readExternal(in: ObjectInput) { houseId = in.readInt() }
  protected def readResolve(): Object = { HouseSingleton.getHouseById(houseId) }
}

sealed trait ListenerBuilder
class CustomListener(f: ⇒ HouseEventListener) extends ListenerBuilder { def apply(): HouseEventListener = f }
case class OpponentListener(f: HouseEventListener ⇒ HouseEventListener) extends ListenerBuilder

object Houses {
  val basicCostFunc = { i: Int ⇒ i + 1 }
  def manaGens = List((0, 3), (1, 5), (3, 5))
}

// hacks for serialization
object HouseSingleton extends Houses

class Houses
    extends Fire with Water with Air with EarthHouse {
  import CardSpec._
  import GameCardEffect._

  val darkPriest = DarkPriest
  val darksider = Darksider
  val dudeMancer = new DudeMancer
  val dreamweaver = new Dreamweaver
  val element = new Elementalist
  val entomologist = Entomologist
  val fairy = new FairyKnight
  val highPriest = HighPriest
  val junkMage = new JunkMage
  val lostChurch = LostChurch
  val moutainKing = new MoutainKing
  val sb = new SB
  val shaman = Shaman
  val sower = new Sower
  val trooper = new Trooper
  val vampire = new Vampire
  val warp = new Warp
  val wind = MasterOfWind
  val zenMage = new ZenMage
  //  val test = new TestMage

  val base = List(Fire, Water, Air, Earth)
  val sinist = List(darkPriest.DarkPriest, darksider.Darksider, element.Elementalist, fairy.Fairy, highPriest.HighPriest, moutainKing.MoutainKing, shaman.Shaman, sower.Sower, vampire.Vampire, wind.Wind)
  val others = List(dreamweaver.Dreamweaver, dudeMancer.Dude, entomologist.Entomologist, trooper.Trooper)
  val bs = List(junkMage.Junk, lostChurch.LostChurch, sb.SB, warp.Warp, zenMage.Zen)
  val special = sinist ++ others ++ bs

  val specialNames: Set[String] = special.map(_.name)(breakOut)
  val specialByName: Map[String, House] = special.map { c ⇒ (c.name, c) }(breakOut)
  private val allHouses = base ++ special
  private val allCards = {
    (allHouses.flatMap(_.cards)
      ++ shaman.additionalCards
      ++ lostChurch.additionalCards
      ++ highPriest.additionalCards
      ++ sb.additionalCards)
  }

  val getHouseById: Map[Int, House] = allHouses.map(h ⇒ h.houseId -> h)(breakOut)
  def getCardById(id: Int): Card = allCards.find(_.id == id) getOrElse sys.error(s"card id $id not found ")

  def isSpecial(house: House) = specialNames contains house.name

  base.zipWithIndex foreach { case (house, index) ⇒ house.initCards(Houses.basicCostFunc) }
}
