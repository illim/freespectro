package priv.sp.bot

import priv.sp._
import priv.sp.update._
import collection._
import util.Random

// bs magic number(i'm bored)
object CardStats {
  val maybe = 0.7f
  val givemeachance = 0.2f
  val convinceditsbetter = 3
  val hoursofpratice = 100
  @inline def letsayhewaitforthis(card : Card, score : Float) = {
    card.cost * score
  }
}

// stats for policy to be less random
class CardStats(initState : GameState, val playerId : PlayerId, bot : Bot) {
  import CardStats._
  val isHighBetter = playerId == bot.botPlayerId

  case class CardStat(card : Card, isHypothetical : Boolean, var total : Float = 0f , var nbUsed : Int = 0){
    val fact = if (isHypothetical) maybe else 1f
    var score = 1f

    // retarded bs map total/nbUsed to R+
    def refreshScore(){
      score = letsayhewaitforthis(card, fact * (if (nbUsed == 0) givemeachance else if (total == 0) 0f else {
        if (isHighBetter){
          if (total <0) - nbUsed/ (nbUsed + total) else 1 + total / nbUsed
        } else {
          if (total <0) 1-(total / nbUsed) else nbUsed / (nbUsed + total)
        }
      }))
    }

    final override def toString() = s"CardStat($total, $nbUsed, $score)"
  }

  var stats = immutable.Map.empty[Card, CardStat]

  initState.players(playerId).desc.get.houses.foreach{ h =>
    h.cards.foreach{ cardDesc =>
      val isHypothetical = (playerId != bot.botPlayerId && cardDesc.card.cost < 9 && !bot.knownCards.exists(_._1 == cardDesc.card))
      stats += (cardDesc.card -> CardStat(cardDesc.card, isHypothetical))
    }
  }

  def getRandomCard(p : PlayerState) : Option[CardDesc] = {
    val houseDescs = p.desc.get.houses
    var cards = immutable.Map.empty[CardDesc, CardStat]
    houseDescs.foreach{ h =>
      val houseState = p.houses(h.house.houseIndex)
      h.cards.foreach{ c =>
        if (c.isAvailable(houseState)){
          stats.get(c.card).foreach{ stat => // todo update list
            stat.refreshScore()
            cards += (c -> stat)
          }
        }
      }
    }
    val cs = cards.size
    val total = (0d /: cards){ (tot, c) =>
      tot + c._2.score
    }
    val mean = total/cs
    val filtereds = ((0d, List.empty[(CardDesc, Double)]) /: cards){ case (current @ (tot, acc), (c, stat)) =>
      val newsc = if (stat.nbUsed < hoursofpratice) stat.score else math.pow(stat.score / mean, convinceditsbetter)
      (tot + newsc, (c, newsc) :: acc)
    }
    //println("sig " + sig + ", discard " + (cs - cards.size))
    val r = Random.nextFloat * filtereds._1
    var res = Option.empty[CardDesc]
    var acc = 0d
    var ite = filtereds._2.iterator
    while(res.isEmpty && ite.hasNext){
      val c = ite.next
      acc += c._2
      if (acc > r){
        res = Some(c._1)
      }
    }
    res
  }

  def update(reward : Float, cards : List[Card]){
    //if (playerId == 0){    println("update " +playerId + ":"+ reward + " for " + cards)    }
    cards.foreach{ c =>
      stats.get(c).foreach{ stat =>
        stat.total += reward
        stat.nbUsed += 1
      }
    }
  }
}
