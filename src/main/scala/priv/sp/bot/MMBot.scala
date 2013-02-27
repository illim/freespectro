package priv.sp.bot

import priv.sp._
import scalaz._
import annotation.tailrec
import scala.util.control.TailCalls._

// another very stupid bot, but faster using minmax with pruning, horrible mess pretty sure it doesn't work :)
class MMBot(val botPlayerId: PlayerId, val game: Game) extends ExtBot {

  private val maxDepth = 4

  def executeAI(start: GameState) = {
    val s = System.currentTimeMillis()
    val node = Node(start, 0, botPlayerId)
    val loc = loop(Tree(node).loc).result
    val root = loc.root.tree
    val result = root.subForest.foldLeft(Option.empty[Node]) {
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

  private final def loop(treeLoc: TreeLoc[Node]): TailRec[TreeLoc[Node]] = {
    def loopOrGoUp(treeLocOpt: Option[TreeLoc[Node]]): TailRec[TreeLoc[Node]] = {
      treeLocOpt match {
        case None =>
          treeLoc.parent match {
            case None => done(treeLoc)
            case Some(parent) =>
              updateParentStat(treeLoc.getLabel, parent.getLabel)
              tailcall(loop(parent))
          }
        case Some(loc) => tailcall(loop(loc))
      }
    }

    val node = treeLoc.getLabel
    val prune = treeLoc.parent.flatMap(_.parent).flatMap(_.getLabel.score).flatMap{ pscore =>
      node.score.map{ score =>
        if (node.playerId == botPlayerId) {
          score > pscore
        } else {
          score < pscore
        }
      }
    } getOrElse false
    if (node.depth < maxDepth && !treeLoc.hasChildren) {
      val nextNodes = node.commandChoices.map { command =>
        Tree(Node(node.state, node.depth + 1, other(node.playerId), Some(command)))
      }
      loopOrGoUp(treeLoc.setTree(Tree.node(node, nextNodes)).firstChild)
    } else if (prune) {
      loopOrGoUp(None)
    } else {
      val right = if (node.depth > 1) deleteThenRight(treeLoc) else treeLoc.right
      for (child <- right; parent <- treeLoc.parent) updateParentStat(child.getLabel, parent.getLabel)
      loopOrGoUp(right)
    }
  }

  def deleteThenRight[A](treeLoc : TreeLoc[A]) : Option[TreeLoc[A]] = {
    treeLoc.rights match {
      case Stream.cons(t, ts) => Some(TreeLoc.loc(t, Stream.empty, ts, treeLoc.parents))
      case _ => None
    }
  }

  def updateParentStat(node: Node, parent: Node) = {
    parent.updateScore(node.score.getOrElse(node.getScore))
  }

  case class Node(initState: GameState, depth: Int, playerId: PlayerId, commandOpt: Option[Command] = None) {
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


  case class MinMax(min : Double, max : Double)

  class Stat {
    var kill = 0
    var damage = 0
  }

}
