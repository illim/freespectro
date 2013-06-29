package priv.sp

import house._
import priv.sp.update._
import java.io._

object House {
  val currentId = new java.util.concurrent.atomic.AtomicInteger
}
// eventListener is only used for special houses
case class House(name: String, cards: List[Card], houseIndex : Int = 4, effects : List[CardSpec.PhaseEffect] = Nil, eventListener : Option[ListenerBuilder] = None) extends Externalizable {
  def this() = this(null, Nil)

  var houseId = House.currentId.incrementAndGet()

  def costs = cards.map(_.cost)

  def initCards(costFn: Int => Int) {
    cards.zipWithIndex.foreach { case (c, i) =>
      c.cost = costFn(i)
      c.houseIndex = houseIndex
      c.houseId = houseId
    }
  }

  override def toString() = name
  override def hashCode() : Int = name.hashCode
  override def equals(o : Any) = {
    o match {
      case h : House => h.hashCode() == hashCode()
      case _ => false
    }
  }
  def writeExternal(out : ObjectOutput ){  out.writeInt(houseId) }
  def readExternal(in : ObjectInput  ){  houseId = in.readInt() }
  protected def readResolve() : Object = HouseSingleton.getHouseById(houseId)
}

sealed trait ListenerBuilder
class CustomListener(f: => HouseEventListener) extends ListenerBuilder { def apply() : HouseEventListener = f  }
case class OpponentListener(f: HouseEventListener => HouseEventListener) extends ListenerBuilder

object Houses {
  val basicCostFunc = { i: Int => i + 1 }
  def manaGens = List((0, 3), (1, 5), (3, 5))
}

// hacks for serialization
object HouseSingleton extends Houses

class Houses
  extends Fire with Water with Air with EarthHouse {
  import CardSpec._
  import GameCardEffect._

  val darkPriest  = new DarkPriest
  val dudeMancer  = new DudeMancer
  val dreamweaver = new Dreamweaver
  val element     = new Elementalist
  val junkMage    = new JunkMage
  val lostChurch  = new LostChurch
  val moutainKing = new MoutainKing
  val sower       = new Sower
  val trooper     = new Trooper
  val vampire     = new Vampire
  val warp        = new Warp
  val zenMage     = new ZenMage
//  val test = new TestMage

  val base = List(Fire, Water, Air, Earth)
  val special = List(darkPriest.DarkPriest, dreamweaver.Dreamweaver, dudeMancer.Dude, element.Elementalist, junkMage.Junk, lostChurch.LostChurch, moutainKing.MoutainKing, sower.Sower, trooper.Trooper, vampire.Vampire, warp.Warp, zenMage.Zen)
  val specialNames = special.map(_.name).to[Set]
  val specialByName = special.map{ c => (c.name, c) }.toMap
  private val allHouses = base ++ special
  private val allCards = allHouses.flatMap(_.cards)

  val getHouseById = allHouses.map(h => h.houseId -> h).toMap
  println(getHouseById)
  def getCardById(id : Int) : Card = allCards.find(_.id == id).getOrElse(sys.error(s"card id $id not found "))

  def isSpecial(house : House)= specialNames.contains(house.name)

  base.zipWithIndex.foreach{ case (house, index) => house.initCards(Houses.basicCostFunc) }
}
