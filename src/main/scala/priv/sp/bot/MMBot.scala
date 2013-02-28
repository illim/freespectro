package priv.sp.bot

import priv.sp._
import priv.util._
import scalaz._
import annotation.tailrec
import scala.util.control.TailCalls._

// another very stupid bot, but faster using minmax with pruning, horrible mess pretty sure it doesn't work :)
class MMBot(val botPlayerId: PlayerId, val game: Game) extends ExtBot {

  private val loop = new Loop()

  def executeAI(start: GameState) = {
    val s = System.currentTimeMillis()
    val node = Node(start, botPlayerId)
    val loc = loop(Tree(node).loc)
    val result = loc.subForest.foldLeft(Option.empty[Node]) {
      case (None, childTree) => Some(childTree.rootLabel)
      case (acc @ Some(node), childTree) =>
        if (node.score.get < childTree.rootLabel.score.get)
          Some(childTree.rootLabel)
        else acc
    }

    result.flatMap { node =>
      println("ai spent " + (System.currentTimeMillis() - s) + ", sc : " + node.score.get)
      node.commandOpt
    }
  }

  class Loop extends BotTreeLoop[Node] {
    val maxDepth = 4

    def getNextsLabel(label : Node) : Stream[Tree[Node]] = label.commandChoices.map { command =>
      Tree(Node(label.state, other(label.playerId), Some(command)))
    }

    def isPrunable(treeLoc : TreeLoc[Node]) : Boolean = {
      treeLoc.parent.flatMap(_.parent).flatMap(_.getLabel.score).flatMap{ pscore =>
        treeLoc.parent.flatMap(_.getLabel.score).map{ score =>
          if (treeLoc.getLabel.playerId == botPlayerId) {
            score > pscore
          } else {
            score < pscore
          }
        }
      } getOrElse false
    }


    def updateLabelOnLeave(label : Node, parentLabel : Node) = parentLabel.updateScore(label.score.getOrElse(label.getScore))
  }

  case class Node(initState: GameState, playerId: PlayerId, commandOpt: Option[Command] = None) {
    lazy val state = commandOpt.map(cmd => simulateCommand(initState, cmd)) getOrElse initState
    var score = Option.empty[Double]

    def updateScore(v : Double){
      score = Some(score match {
        case None => v
        case Some(x) => if (playerId == botPlayerId) math.max(x, v) else math.min(x, v)
      })
    }

    def commandChoices: Stream[Command] = getCommandChoices(state, playerId)

    def getScore = {
      val stats = playerIds.map{ playerId =>
        val stat= new Stat
        if (state.players(other(playerId)).life <= 0) stat.kill += 1
        else {
          stat.damage = state.players(playerId).life - initState.players(playerId).life
        }
        stat
      }

      val v = stats(botPlayerId)
      val v2 = stats(other(botPlayerId))
      ((v.kill - v2.kill) + (v.damage - v2.damage) * 0.1) // stupid heuristic
    }
  }

  class Stat {
    var kill = 0
    var damage = 0
  }

}

// full of side effect horror
trait BotTreeLoop[A] {
  def maxDepth:Int

  var depth = 0

  final def apply(start: TreeLoc[A]) : Tree[A] = {
    var treeLoc = start
    var end = false
    while (!end) {
      val label = treeLoc.getLabel
      val prune = isPrunable(treeLoc)
      if (depth == maxDepth) {
        for (parent <- treeLoc.parent) updateLabelOnLeave(label, parent.getLabel)
      }
      val nextTreeLoc = {
        if (depth < maxDepth && !treeLoc.hasChildren) {
          val nextLabels = getNextsLabel(label)
          if (nextLabels.nonEmpty) {
            depth += 1
          }
          treeLoc.setTree(Tree.node(label, nextLabels)).firstChild
        } else if (prune) {
          None
        } else {
          val right = if (depth > 1) Utils.deleteThenRight(treeLoc) else treeLoc.right
          right
        }
      }

      nextTreeLoc match {
        case None =>
          treeLoc.parent match {
            case None => end = true
            case Some(parent) =>
              depth -= 1
              treeLoc = parent
              for (parent <- treeLoc.parent) updateLabelOnLeave(treeLoc.getLabel, parent.getLabel)
          }
        case Some(loc) => treeLoc = loc
      }
    }
    treeLoc.root.tree
  }


  def getNextsLabel(label : A) : Stream[Tree[A]]

  def isPrunable(treeLoc : TreeLoc[A]) : Boolean

  def updateLabelOnLeave(label : A, parentLabel : A)

}
