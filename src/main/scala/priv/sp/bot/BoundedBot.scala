package priv.sp.bot

import scala.util.Random
import priv.sp._
import priv.sp.update._
import priv.util._
import scalaz._
import collection._

// another stupid bot, faster because bounded in time, trying to use uct.
// select best reward and not numsim due to boost(leafed node when early win/loss detected)
// not using flat uct for opponent next move fairness?
class BoundedBot(val botPlayerId: PlayerId, val gameDesc : GameDesc, val spHouses : Houses, heurisChoice : Int = 3) extends Bot {
  val heuris = heurisChoice match {
    case 0 => new LifeManaRatioHeuris(botPlayerId)
    case 1 => new LifeHeuris(botPlayerId)
    case 2 => new MultiRatioHeuris(botPlayerId, "Apprentice", useKillValueRatio = true)
    case 3 => new MultiRatioHeuris(botPlayerId, "Junior", useOppManaRatio = true, useKillValueRatio = true, useBoardRatio = true)
//    case 3 => new MultiRatioHeuris(botPlayerId, "Rush4life", useManaRatio = false)
  }

  def executeAI(start: GameState) = {
    val st = k.ripDescReader(start)
    heuris.init(st)
    initGameUpdater(start)
    new BoundedBotAI(botPlayerId, st, this, heuris).execute()
  }
}

class BoundedBotAI(botPlayerId: PlayerId, start : GameState, bot : Bot, heuris : Heuris) {
  val duration = 4000
  var defaultPolicyMaxTurn = 10
  val expansionTreeMaxDepth = 2  // todo shuffle the nodes before increase maxDepth?
  val boostFactor = 3

  val human = other(botPlayerId)
  val selector = new Selector()
  val perfStat = new PerfStat()
  val cardStats = playerIds.map{ p => new CardStats(start, p, p == human) }
  val choices = new Choices(bot, cardStats)

  def execute() = {
    val startTime = System.currentTimeMillis
    val end = startTime + duration
    val node = Node(start, WaitPlayer(botPlayerId), Nil, None)
    val rootLoc = Tree(node).loc
    var next = Option(rootLoc)
    var last = rootLoc
    var i = 0
    while(System.currentTimeMillis < end && next.isDefined){
      next = selector.treePolicy(last).map{ selected =>
        defaultPolicy(selected.getLabel)
        selected.root
      }
      if (next.isDefined){
        last = next.get
      }
      i+=1
    }
    val result = last.tree.subForest.foldLeft(Option.empty[Node]) {
      case (None, childTree) =>
        println(childTree.rootLabel.statString)
        Some(childTree.rootLabel)
      case (acc @ Some(node), childTree) =>
        val child = childTree.rootLabel
        println(child.statString)
        if (node.getAvgReward < child.getAvgReward)
          Some(childTree.rootLabel)
        else acc
    }
    //last.tree.draw(Show.showFromToString[Node]).foreach(println _)
    //cardStats.foreach(c => println("stats=" + c.stats.toList.sortBy(_._2.score).mkString("\n")))
    result.flatMap { node =>
      println(s"ai spent ${(System.currentTimeMillis() - startTime)}, numSim : ${node.numSim}, ${perfStat} , ${i} iterations")
      node.commandOpt
    }
  }

  def defaultPolicy(node : Node) = {
    var nbStep = 0
    var state = node.state
    var end = state.checkEnded
    var player = node.outTransition.playerId
    perfStat.nbdefpol += 1
    bot.updater.resetStats()
    var botCards = List.empty[Card]
    var oppCards = List.empty[Card]
    //println("path " + node.path+ "/" + state.players.map(_.life))
    while(nbStep < defaultPolicyMaxTurn && end.isEmpty){
      val nextCommand = choices.getRandomMove(state, player)
      nextCommand.foreach{ c =>
        if (player == human){
          oppCards = c.card :: oppCards
        } else {
          botCards = c.card :: botCards
        }
      }
      val (gameState, transition) = bot.simulateCommand(state, player, nextCommand)
      state = gameState
      //print("- def:"+nextCommand + "/" + player + "/" + state.players.map(_.houses))
      player = transition.playerId
      nbStep += 1
      end = state.checkEnded
    }
    //println("end " +nbStep+","+ end)
    val reward = node.updateStatsFrom(state, end, playerStats = Some(bot.updater.stats))
    cardStats(botPlayerId).update(reward, botCards)
    cardStats(human).update(reward, oppCards)
  }

  class Selector extends SelectExpandLoop[Node] {
    val maxDepth = expansionTreeMaxDepth

    def getNexts(label : Node) : Stream[Tree[Node]] = {
      val path = label :: label.path
      label.state.checkEnded match {
        case Some(p) =>
          Stream.Empty
        case None =>
          new Stream.Cons(
            Tree(Node(label.state, label.outTransition, path, None, label.depth + 1)),
            label.commandChoices.map { command =>
              Tree(Node(label.state, label.outTransition, path, Some(command), label.depth + 1))
            })
      }
    }

    def select(x : Node, y : Node, fairOnly : Boolean) : Boolean = if (fairOnly) x.getFair > y.getFair else x.getUct > y.getUct
  }

  case class Node(initState: GameState, transition : Transition, path : List[Node], commandOpt: Option[Command], depth : Int = 0) extends LeafableNode {
    var numSim = 0.1f
    var rewards = 0f
    def playerId = transition.playerId
    def isFairOnly : Boolean = playerId == other(botPlayerId)
    def isRoot = depth == 0

    def parent = path.headOption
    def getAvgReward : Float = rewards/numSim
    def getUct : Float =  parent.map{ p =>
      getAvgReward + math.sqrt(2 * math.log(p.numSim)/numSim).floatValue
    }.getOrElse(0f)
    def getFair = parent.map{ p =>
      math.sqrt(2 * math.log(p.numSim)/numSim).floatValue
    }.getOrElse(0f)

    bot.updater.resetStats()
    val (state, outTransition) = if (isRoot) (initState, transition) else {
      perfStat.nbsim += 1
      bot.simulateCommand(initState, playerId, commandOpt) // should be random here
    }
    val nodePlayerStats = bot.updater.stats.map(_.copy())

    //workaround if simulation is out of budget, at least update stat if
    state.checkEnded.foreach{ p =>
      leafed = true
      if (p == other(botPlayerId)){ // FIXME decrease sim time if we aim short time survival
        defaultPolicyMaxTurn = 2
      }
      updateStatsFrom(state, Some(p), boost = boostFactor * (1 + expansionTreeMaxDepth - depth)) // FIXME not great
    }

    def commandChoices: Stream[Command] = choices.getNexts(state, outTransition.playerId)
    def updateStatsFrom( st : GameState, end : Option[PlayerId], boost : Int = 1, playerStats : Option[List[PlayerStats]] = None) = {
      numSim += 1
      val stats = playerIds.map{ i =>
        val s = nodePlayerStats(i)
        playerStats.map(_(i) + s).getOrElse(s)
      }
      val reward = boost * end.map{ p =>
        if (p == botPlayerId) 1f else -1f
      }.getOrElse(0.01f * heuris(st, stats, depth))
      rewards += reward
      backPropagate(reward)
      reward
    }
    private def backPropagate(reward : Float){
      path.foreach{ node =>
        node.numSim += 1
        node.rewards += reward
      }
    }
    def stringPath = path.collect{ case p if p.commandOpt.isDefined=> p.commandOpt.get.card.name}
    final override def toString() = s"Node($commandOpt : $getUct, avgRwd=$getAvgReward, nSim=$numSim, $stringPath, ${state.players.map(_.life)})"
    def statString = s"Node($commandOpt : $getUct, avgRwd=$getAvgReward, nSim=$numSim)"
  }

  case class PerfStat( var nbsim : Int = 0, var nbdefpol : Int = 0)
}

trait LeafableNode{
  var leafed = false
  def isFairOnly : Boolean
  def depth : Int
}

// full of side effect horror
/**
 * FIXME balancing choice when wait player twice
 */
trait SelectExpandLoop[A <: LeafableNode] {

  def maxDepth:Int

  def getNexts(label : A) : Stream[Tree[A]]

  def select(x :A, y : A, fairOnly : Boolean) : Boolean

  final def treePolicy(start: TreeLoc[A]) : Option[TreeLoc[A]] = {
    var treeLoc = start
    var end = false
    while (!end) {
      val depth = treeLoc.getLabel.depth
      if (depth == maxDepth){
        end = true
      } else {
        val label = treeLoc.getLabel
        val nextTreeLoc = {
          if (depth < maxDepth && !treeLoc.hasChildren && !label.leafed) {
            expand(treeLoc)
          } else {
 // if there's no move available, will is there a risk to come back here until the select score change? (maybe a flag to not reselect this?)
            if (depth == maxDepth - 1
                || label.leafed) { // workaround todo fix this case
              end = true
            }
            treeLoc.firstChild.map{ child =>
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
    treeLoc.setTree(Tree.node(treeLoc.getLabel, nextLabels)).firstChild
  }

  private def selectChild(treeLoc : TreeLoc[A]) : TreeLoc[A] = {
    val isFairOnly = treeLoc.getLabel.isFairOnly
    var result = treeLoc
    var next = treeLoc.right
    while(next.isDefined){
      if (result.getLabel.leafed || select(next.get.getLabel, result.getLabel, isFairOnly)) {
        result = next.get
      }
      next = next.get.right
    }
    result
  }
}
