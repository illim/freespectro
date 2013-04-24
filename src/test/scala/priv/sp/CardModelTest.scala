package priv.sp

import util.Random
import scalaz._
import org.scalatest._
import org.scalatest.matchers._
/**
class CardModelSpec extends FlatSpec with ShouldMatchers {
  val spHouses = new Houses

  "A solver" should "enable to shuffle cards" in {
    val cardModel = CardModel.build(spHouses)
    val shuffler = new CardShuffler(cardModel)
    println(shuffler.solve())
    cardModel.houses.foreach(h => println(h.cards.map(_.value)))

    val manaModel = new ManaModel(cardModel)
    val manaShuffler = new ManaShuffler(manaModel, true)
    println(manaShuffler.solve())
    println(manaModel.manas.map(_.value))
  }

  "A solver" should "enable to shuffle cards with a filter" in {
    val exclusion = Random.shuffle(spHouses.Fire.cards).take(2).to[Array]
    val player = PlayerDesc(Array(PlayerHouseDesc(spHouses.Fire, exclusion)))
    val cardRangeFilter = new CardModel.ExcludePlayerCards(player)
    val cardModel = CardModel.build(new Houses, getCardRange = cardRangeFilter)
    val shuffler = new CardShuffler(cardModel)
    println("excluding " + exclusion.toList)
    println(shuffler.solve())
    cardModel.fire.map(_.value).foreach{ x =>
      assert(!exclusion.contains(x))
    }
    cardModel.houses.foreach(h => println(h.cards.map(_.value)))
  }

}
*/
