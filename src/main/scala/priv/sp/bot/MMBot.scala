package priv.sp.bot

import priv.sp._
import priv.util._
import scalaz._
import annotation.tailrec
import scala.util.control.TailCalls._

// another very stupid bot, but a bit less slow using minmax with pruning, horrible mess pretty sure it doesn't work :)
class MMBot(val botPlayerId: PlayerId, val game: Game) extends ExtBot {

  def executeAI(start: GameState) = {
    new MMBotAI(botPlayerId, start, this).execute()
  }
}

class MMBotAI(botPlayerId: PlayerId, start : GameState, bot : ExtBot) {
  private val loop = new Loop()
  private var stat = new Stat()
  private var choices = new Choices(bot)

  def execute() : Option[Command] = {
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
      println("ai spent " + (System.currentTimeMillis() - s) + ", sc : " + node.score.get + ", "+ stat)
      node.commandOpt
    }
  }

  class Loop extends BotTreeLoop[Node] {
    val maxDepth = 4

    def getNexts(label : Node) : Stream[Tree[Node]] = label.commandChoices.map { command =>
      Tree(Node(label.state, other(label.playerId), Some(command)))
    }

    def isPrunable(treeLoc : TreeLoc[Node]) : Boolean = {
      stackGet(2).flatMap(_.getLabel.score).flatMap{ pscore =>
        stackGet(1).get.getLabel.score.filter{ score =>
          if (treeLoc.getLabel.playerId == botPlayerId) {
            score > pscore
          } else {
            score < pscore
          }
        }
      }.isDefined
    }


    def propagate(label : Node, parentLabel : Node) = parentLabel.updateScore(label.score.getOrElse(label.getScore))
  }

  case class Node(initState: GameState, playerId: PlayerId, commandOpt: Option[Command] = None) {
    lazy val state = commandOpt.map{ cmd =>
      stat.nbsim += 1
      bot.simulateCommand(initState, cmd)
    } getOrElse initState
    var score = Option.empty[Double]

    def updateScore(v : Double){
      score = Some(score match {
        case None => v
        case Some(x) => if (playerId == botPlayerId) math.max(x, v) else math.min(x, v)
      })
    }

    def commandChoices: Stream[Command] = choices.getNexts(state, playerId)

    def getScore = {
      def stats(playerId : PlayerId) = {
        if (state.players(other(playerId)).life <= 0) new NodeStat(kill = 1)
        else new NodeStat(damage = state.players(playerId).life - start.players(playerId).life)
      }
      val v = stats(botPlayerId)
      val v2 = stats(other(botPlayerId))

      ((v.kill - v2.kill) + (v.damage - v2.damage) * 0.01) // stupid heuristic
    }

    private class NodeStat(val kill:Int = 0, val damage : Int = 0)
  }

  case class Stat( var nbsim : Int = 0)
}

// full of side effect horror
trait BotTreeLoop[A] {

  def maxDepth:Int

  def getNexts(label : A) : Stream[Tree[A]]

  def isPrunable(treeLoc : TreeLoc[A]) : Boolean

  // used to propagate the min/max values when evaluating a leaf or going up
  def propagate(label : A, parentLabel : A)

  var depth = 0

  // useless perf shit (todo not using a tree finally?)
  var stack = Vector[TreeLoc[A]]()
  def stackGet(index : Int) = if (index > stack.size - 1) None else Some(stack(index))

  final def apply(start: TreeLoc[A]) : Tree[A] = {
    var treeLoc = start
    var end = false
    while (!end) {
      stack = treeLoc +: stack
      val label = treeLoc.getLabel
      val prune = isPrunable(treeLoc)
      if (depth == maxDepth) {
        propagate(label, stack(1).getLabel)
      }
      val nextTreeLoc = {
        if (depth < maxDepth && !treeLoc.hasChildren) {
          val nextLabels = getNexts(label)
          if (nextLabels.nonEmpty) {
            depth += 1
          }
          treeLoc.setTree(Tree.node(label, nextLabels)).firstChild
        } else if (prune) {
          stack = stack.tail
          None
        } else {
          stack = stack.tail
          if (depth > 1) Utils.deleteThenRight(treeLoc) else treeLoc.right
        }
      }

      nextTreeLoc match {
        case None =>
          treeLoc.parent match {
            case None => end = true
            case Some(parent) =>
              stack = stack.tail
              depth -= 1
              treeLoc = parent
              if (stack.size > 0)
                propagate(treeLoc.getLabel, stack(0).getLabel)
          }
        case Some(loc) => treeLoc = loc
      }
    }
    treeLoc.root.tree
  }
}
