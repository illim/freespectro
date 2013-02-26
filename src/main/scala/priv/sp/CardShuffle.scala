package priv.sp

/**
 * TODO :
 * 2 healers, 1 direct spell, manual exclusions
 */

import util.Random

class CardShuffle(sp: SpWorld) {
  import sp.houses._

  def get() = {
    val p1 = createOnePlayer(list)
    val p2 = createOnePlayer(filterHouses(p1._1)(list))
    List(p1, p2)
  }

  def filterHouses(p: PlayerDesc)(houses: List[House]) = {
    houses.zipWithIndex.map {
      case (h, idx) =>
        val p1Cards = p.houses(idx).cards
        h.copy(cards = h.cards.filter { c => !p1Cards.contains(c) })
    }
  }

  def createOnePlayer(houses: List[House]) = {
    def eval = {
      val fromHouses = (0 to 4).map(i => from(houses(i))).toList
      (PlayerDesc(fromHouses.map(_._1)), fromHouses.map(_._2))
    }
    var s = eval
    while (!(hasOneWipe(s._1) && hasOneManaGen(s._1))) {
      s = eval
    }
    s
  }

  private def from(house: House) = {
    val mana = if (isSpecial(house)) 2 else Random.nextInt(3) + 3
    (PlayerHouseDesc(house, randomize(house).sortBy(_.cost)), HouseState(mana))
  }

  private def randomize(house: House) = {
    import Random.shuffle
    val (cut1, cut2) = if (isSpecial(house)) (5, 7) else (7, 11)
    val (c1, ctemp) = house.cards.partition(_.cost < cut1)
    val (c2, c3) = ctemp.partition(_.cost < cut2)
    shuffle(c1).take(2) ++ shuffle(c2).take(1) ++ shuffle(c3).take(1)
  }

  private val fwipes = List(6, 9)
  private val awipes = List(8)
  private def hasOneWipe(player: PlayerDesc) = {
    (player.houses(0).cards.count(card => fwipes.contains(card.cost))
      + player.houses(2).cards.count(card => awipes.contains(card.cost))) == 1
  }

  private def hasOneManaGen(player: PlayerDesc) = {
    def toi(b: Boolean) = if (b) 1 else 0
    def hasCard(house: Int, cost: Int) = toi(player.houses(house).cards.exists(card => card.cost == cost))
    (hasCard(0, 3) + hasCard(1, 5) + hasCard(3, 5)) == 1
  }

}