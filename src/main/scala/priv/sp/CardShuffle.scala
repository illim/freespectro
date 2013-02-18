package priv.sp

/**
 * TODO :
 * 2 healers, 1 direct spell, manual exclusions
 */

import util.Random

object CardShuffle {

  def apply() = {
    def houseFilter(house: House)(f: Card => Boolean) = house.copy(cards = house.cards.filter(f))
    val baseHouses = List(Houses.Fire, Houses.Water, Houses.Air, Houses.Earth, Houses.Mecanic)
    val p1 = createOnePlayer(baseHouses)
    val p2 = createOnePlayer(baseHouses.zipWithIndex.map {
      case (h, idx) =>
        val p1Cards = p1.houses(idx).cards
        houseFilter(h) { c => !p1Cards.contains(c) }
    })
    List(p1, p2)
  }

  private def createOnePlayer(houses: List[House]) = {
    def eval = new PlayerState((0 to 4).map(i => from(houses(i))).toList)

    var r = eval
    while (!(hasOneWipe(r) && hasOneManaGen(r))) {
      r = eval
    }
    r
  }

  private def from(house: House) = {
    val mana = if (house.isSpecial) 2 else Random.nextInt(3) + 3
    HouseState(house, randomize(house).sortBy(_.cost), mana)
  }

  private def randomize(house: House) = {
    import Random.shuffle
    val (cut1, cut2) = if (house.isSpecial) (5, 7) else (7, 11)
    val (c1, ctemp) = house.cards.partition(_.cost < cut1)
    val (c2, c3) = ctemp.partition(_.cost < cut2)
    shuffle(c1).take(2) ++ shuffle(c2).take(1) ++ shuffle(c3).take(1)
  }

  private val fwipes = List(6, 9)
  private val awipes = List(8)
  private def hasOneWipe(player: PlayerState) = {
    (player.houses(0).cards.count(card => fwipes.contains(card.cost))
      + player.houses(2).cards.count(card => awipes.contains(card.cost))) == 1
  }

  private def hasOneManaGen(player: PlayerState) = {
    def toi(b: Boolean) = if (b) 1 else 0
    def hasCard(house: Int, cost: Int) = toi(player.houses(house).cards.exists(card => card.cost == cost))
    (hasCard(0, 3) + hasCard(1, 5) + hasCard(3, 5)) == 1
  }

}