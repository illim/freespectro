package priv.sp.bot

import priv.sp._
import priv.util._
import scalaz._
import annotation.tailrec
import scala.util.control.TailCalls._

// another slow dumb bot with stupid heuristic who minimize life loss :(
// todo optim prune
class DummyBot2(val botPlayerId: PlayerId, val game: Game) extends ExtBot {

  private val maxDepth = 4

  def executeAI(start: GameState) = {
    val s = System.currentTimeMillis()
    val node = Node(start, 0, botPlayerId)
    val loc = loop(Tree(node).loc).result
    val root = loc.root.tree
    val result = root.subForest.foldLeft(Option.empty[Node]) {
      case (None, childTree) => Some(childTree.rootLabel)
      case (acc @ Some(node), childTree) =>
        if (node.stats.score(botPlayerId) < childTree.rootLabel.stats.score(botPlayerId))
          Some(childTree.rootLabel)
        else acc
    }

    result.flatMap { node =>
      println("ai spent " + (System.currentTimeMillis() - s) + ", sc : " + node.stats.score(botPlayerId))
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
    if (node.depth < maxDepth && !treeLoc.hasChildren) {
      val nextNodes = node.commandChoices.map { command =>
        Tree(Node(node.state, node.depth + 1, other(node.playerId), Some(command)))
      }
      loopOrGoUp(treeLoc.setTree(Tree.node(node, nextNodes)).firstChild)
    } else {
      val right = if (node.depth > 1) Utils.deleteThenRight(treeLoc) else treeLoc.right
      for (child <- right; parent <- treeLoc.parent) updateParentStat(child.getLabel, parent.getLabel)
      loopOrGoUp(right)
    }
  }

  def updateParentStat(node: Node, parent: Node) = {
    import node.state
    import parent.stats

    stats.count += 1
    val stat = stats(node.playerId)
    if (state.players(other(node.playerId)).life <= 0) stat.kill += 1
    else {
      playerIds.foreach { playerId =>
        stats(playerId).damage = state.players(playerId).life - node.initState.players(playerId).life
      }
    }
  }

  case class Node(initState: GameState, depth: Int, playerId: PlayerId, commandOpt: Option[Command] = None) {
    lazy val state = commandOpt.map(cmd => simulateCommand(initState, cmd)) getOrElse initState
    val stats = new Stats

    def commandChoices: Stream[Command] = getCommandChoices(state, playerId)
  }

  class Stats {
    val value = playerIds.map(_ => new Stat)
    var count = 0

    def apply(playerId: PlayerId) = value(playerId)
    def score(playerId: PlayerId) = {
      val v = value(playerId)
      val v2 = value(other(playerId))
      ((v.kill - v2.kill) + (v.damage - v2.damage) * 0.1) / count // stupid heuristic
    }
  }

  class Stat {
    var kill = 0
    var damage = 0
  }

}
