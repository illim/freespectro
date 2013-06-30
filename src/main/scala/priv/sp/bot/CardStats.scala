package priv.sp.bot

import priv.sp._
import collection._
import util.Random

// stats for policy to be less random
class CardStats(initState : GameState, val playerId : PlayerId, isHighBest : Boolean) {

  case class CardStat(var total : Float = 0f , var nbUsed : Int = 0){
    // retarded bs
    def score =
      if (nbUsed == 0) 0.2f else if (total == 0) 0f else {
        val fact = if (isHighBest){
          if (total <0) - nbUsed/ total else 1 + total / nbUsed
        } else {
          if (total <0) 1-(total / nbUsed) else nbUsed /total
        }
        fact + (math.sqrt(2 * math.log(0.1f + nbUsed)/nbUsed)).floatValue // same as uct
      }
    final override def toString() = s"CardStat($total, $nbUsed, $score)"
  }

  var stats = immutable.Map.empty[Card, CardStat]

  initState.players(playerId).desc.get.houses.foreach{ h =>
    h.cards.foreach{ cardDesc =>
      stats += (cardDesc.card -> CardStat())
    }
  }

  def getRandomCard(p : PlayerState) : Option[CardDesc] = {
    val houseDescs = p.desc.get.houses
    var cards = immutable.Map.empty[CardDesc, (CardStat, Float)]
    houseDescs.foreach{ h =>
      val houseState = p.houses(h.house.houseIndex)
      h.cards.foreach{ c =>
        if (c.isAvailable(houseState)){
          stats.get(c.card).foreach{ stat => // todo update list
            cards += (c -> (stat, c.card.cost * stat.score ))
          }
        }
      }
    }
    val cs = cards.size
    val total = (0f /: cards){ (tot, c) =>
      tot + c._2._2
    }
    val m = total/cs
    val sigtot = (0d /: cards){ (s, c) =>
      s + math.pow(m - c._2._2, 2)
    }
    val sig = math.sqrt(sigtot / cs) / 2
    cards = cards.filter{ case (c, (s, sc)) => sc > m - sig || s.nbUsed < 100   }
    //println("sig " + sig + ", discard " + (cs - cards.size))
    val r = Random.nextFloat * total
    var res = Option.empty[CardDesc]
    var acc = 0f
    var ite = cards.iterator
    while(res.isEmpty && ite.hasNext){
      val c = ite.next
      acc += c._2._2
      if (acc > r){
        res = Some(c._1)
      }
    }
    res
  }

  def update(reward : Float, cards : List[Card]){
    //println("update " +playerId + ":"+ reward + " for " + cards)
    cards.foreach{ c =>
      stats.get(c).foreach{ stat =>
        stat.total += reward
        stat.nbUsed += 1
      }
    }
  }
}
