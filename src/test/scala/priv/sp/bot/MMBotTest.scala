package priv.sp.bot

import collection.mutable._
import scalaz._
import org.scalatest._
import org.scalatest.matchers._

class MMBotSpec extends FlatSpec with ShouldMatchers {

  "A loop" should "enable to follow minimax" in {
    val loop = new TestLoop
    val result = loop(Tree(Node(1)).loc)
    result.rootLabel.score.get should equal(3)
  }

  val data = new HashMap[Int, Set[Int]]() with MultiMap[Int, Int]
  data ++= List(1 -> Set(2, 3, 4),
    2 -> Set(5, 6, 7),
    3 -> Set(8, 9, 10),
    4 -> Set(11, 12, 13))
  val scoreData = Map(
    5 -> 3, 6 -> 12, 7 -> 8,
    8 -> 2, 9 -> 13, 10 -> 1,
    11 -> 14, 12 -> 5, 13 -> 2)


  class TestLoop extends BotTreeLoop[Node] {
    val maxDepth = 2

    def getNextsLabel(label : Node) : Stream[Tree[Node]] = data(label.id).map{ id => Tree(Node(id)) }.toStream

    def isPrunable(treeLoc : TreeLoc[Node]) : Boolean = {
      treeLoc.parent.flatMap(_.parent).flatMap(_.getLabel.score).flatMap{ pscore =>
        treeLoc.parent.flatMap(_.getLabel.score).map{ score =>
          if (depth % 2 == 0) {
            score < pscore
          } else {
            score > pscore
          }
        }
      } getOrElse false
    }

    def updateLabelOnLeave(label : Node, parentLabel : Node) = parentLabel.updateScore(depth ,label.score.getOrElse(label.getScore))
  }

  case class Node(id : Int){
    var score = Option.empty[Int]
    def getScore = scoreData(id)

    def updateScore(depth : Int, v : Int){
      score = Some(score match {
        case None => v
        case Some(x) => if (depth % 2 == 0) math.min(x, v) else math.max(x, v)
      })
      println("update score of " + id + " for " + v + " to " + score + " depth"+depth)
    }
  }

}
