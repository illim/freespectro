package priv.sp.bot

import priv.sp._
import priv.util._
import scalaz._
import collection._
import scala.util.control.TailCalls._

// another stupid bot, faster because bounded in time, trying to use uct.
class BoundedBot(val botPlayerId: PlayerId, val game: Game) extends Bot {
  def executeAI(start: GameState) = {
    new BoundedBotAI(botPlayerId, start, this).execute()
  }
}

class BoundedBotAI(botPlayerId: PlayerId, start : GameState, bot : Bot) {
  val duration = 4000
  val defaultPolicyMaxTurn = 15
  val expansionTreeMaxDepth = 2  // todo shuffle the nodes before increase maxDepth?

  val selector = new Selector()
  val perfStat = new PerfStat()
  val choices = new Choices(bot)
  val deltaStart = start.players(botPlayerId).life  -start.players(other(botPlayerId)).life

  def execute() = {
    val startTime = System.currentTimeMillis
    val end = startTime + duration
    val node = Node(start, botPlayerId, Nil)
    val rootLoc = Tree(node).loc
    var next = Option(rootLoc)
    var last = rootLoc
    var i = 0
    while(System.currentTimeMillis < end && next.isDefined){
      next = selector.treePolicy(last).map{ selected =>
        defaultPolicy(selected.getLabel)
        selected.root
      }
      last = next.get
      i+=1
    }
    val result = last.tree.subForest.foldLeft(Option.empty[Node]) {
      case (None, childTree) =>
        println(childTree.rootLabel)
        Some(childTree.rootLabel)
      case (acc @ Some(node), childTree) =>
        val child = childTree.rootLabel
        println(child)
        if (node.numSim < child.numSim)
          Some(childTree.rootLabel)
        else acc
    }

    result.flatMap { node =>
      println(s"ai spent ${(System.currentTimeMillis() - startTime)}, numSim : ${node.numSim}, ${perfStat} , ${i} iterations")
      node.commandOpt
    }
  }

  def defaultPolicy(node : Node) = {
    var nbStep = 0
    var state = node.initState
    var end = state.checkEnded
    var player = node.playerId
    perfStat.nbdefpol += 1
    while(nbStep < defaultPolicyMaxTurn && end.isEmpty){
      val nextCommand = choices.getRandomMove(state, player)
      //println("def:"+nextCommand)
      state = bot.simulateCommand(state, player, nextCommand)
      player = other(player)
      nbStep += 1
      end = state.checkEnded
    }
    node.updateStatsFrom(state, end)
  }

  class Selector extends SelectExpandLoop[Node] {
    val maxDepth = expansionTreeMaxDepth

    def getNexts(label : Node) : Stream[Tree[Node]] = {
      val path = label :: label.path
      label.state.checkEnded match {
        case Some(p) =>
          label.leafed = true
          label.updateStatsFrom(label.state, Some(p), boost = label.commandChoices.size * (1 + maxDepth - depth)) // FIXME not great
          Stream.Empty
        case None =>
          label.commandChoices.map { command =>
            Tree(Node(label.state, other(label.playerId), path, Some(command)))
          }
      }
    }

    def select(x : Node, y : Node) : Boolean = x.getUct > y.getUct
  }

  case class Node(initState: GameState, playerId: PlayerId, path : List[Node], commandOpt: Option[Command] = None) extends LeafableNode {
    var numSim = 0.1f
    var rewards = 0f

    def parent = path.headOption
    def getAvgReward : Float = rewards/numSim
    def getUct : Float =  parent.map{ p =>
      getAvgReward + math.sqrt(2 * math.log(p.numSim)/numSim).floatValue
    }.getOrElse(0f)

    lazy val state = commandOpt.map{ cmd =>
      perfStat.nbsim += 1
      bot.simulateCommand(initState, cmd) // should be random here
    } getOrElse initState

    def commandChoices: Stream[Command] = choices.getNexts(state, playerId)
    def updateStatsFrom( st : GameState, end : Option[PlayerId], boost : Int = 1){
      numSim += boost
      // stupid heuristic again
      val deltaReward = end.map{ p =>
        if (p == botPlayerId) 1f else -1f
      }.getOrElse {
        val dend = st.players(botPlayerId).life - st.players(other(botPlayerId)).life
        0.01f * (dend - deltaStart)
      }
      rewards += boost * deltaReward
      backPropagate(deltaReward)
    }
    private def backPropagate(deltaReward : Float){
      path.foreach{ node =>
        node.numSim += 1
        node.rewards += deltaReward
      }
    }
    def stringPath = path.collect{ case p if p.commandOpt.isDefined=> p.commandOpt.get.card.name}
    override def toString() = s"Node($commandOpt : $getUct , numSim=$numSim, rewards=$rewards, $stringPath)"
  }

  case class PerfStat( var nbsim : Int = 0, var nbdefpol : Int = 0)
}

trait LeafableNode{
  var leafed = false
}

// full of side effect horror
trait SelectExpandLoop[A <: LeafableNode] {

  def maxDepth:Int

  def getNexts(label : A) : Stream[Tree[A]]

  def select(x :A, y : A) : Boolean

  var depth = 0

  final def treePolicy(start: TreeLoc[A]) : Option[TreeLoc[A]] = {
    depth = 0
    var treeLoc = start
    var end = false
    while (!end) {
      if (depth == maxDepth){
        end = true
      } else {
        val label = treeLoc.getLabel
        val nextTreeLoc = {
          if (depth < maxDepth && !treeLoc.hasChildren && !label.leafed) {
            expand(treeLoc)
          } else {
            if (depth == maxDepth - 1) { // if there's no move available, will is there a risk to come back here until the select score change? (maybe a flag to not reselect this?)
              end = true
            }
            treeLoc.firstChild.map{ child =>
              depth += 1
              treeLoc = selectChild(child)
              treeLoc
            }
          }
        }

        if (! end){
          nextTreeLoc match {
            case None =>
              treeLoc.parent match {
                case None => end = true
                case Some(parent) =>
                  depth -= 1
                  treeLoc = parent
              }
            case Some(loc) => treeLoc = loc
          }
        }
      }
    }
    if (treeLoc ne start){
      Some(treeLoc)
    } else None
  }

  private def expand(treeLoc : TreeLoc[A]) : Option[TreeLoc[A]]= {
    val nextLabels = getNexts(treeLoc.getLabel)
    if (nextLabels.nonEmpty) {
      depth += 1
    }
    treeLoc.setTree(Tree.node(treeLoc.getLabel, nextLabels)).firstChild
  }

  private def selectChild(treeLoc : TreeLoc[A]) : TreeLoc[A] = {
    var result = treeLoc
    var next = treeLoc.right
    while(next.isDefined){
      if (result.getLabel.leafed || select(next.get.getLabel, result.getLabel)) {
        result = next.get
      }
      next = next.get.right
    }
    result
  }
}
