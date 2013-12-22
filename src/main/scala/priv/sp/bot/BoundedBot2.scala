package priv.sp.bot

import scala.util.Random
import priv.sp._
import priv.sp.update._
import priv.util._
import scalaz._
import collection._

class BoundedBot2AI(botPlayerId: PlayerId, start : GameState, bot : Bot, settings : Settings) extends BotTree {
  var defaultPolicyMaxTurn = 10
  val maxDepth = 2
  val human = other(botPlayerId)
  val cardStats = playerIds.map{ p => new CardStats(start, p, bot) }
  val choices = new Choices(bot, cardStats, settings)
  val heuris = new LifeHeuris(botPlayerId, settings)
  heuris.init(start)

  type TreeLabel = Node

  def execute() = {
    val startTime = System.currentTimeMillis
    val end = startTime + settings.duration
    val node = new Tree(Node(start, WaitPlayer(botPlayerId), bot.updater.stats))
    val loc = node.loc
    var continue = true
    var i = 0
    while(System.currentTimeMillis < end && continue){
      treePolicy(loc) match {
          case Some(selected) =>  defaultPolicy(selected)
          case None => continue = false
      }
      i+=1
    }

    val result = node.subforest.foldLeft(Option.empty[TreeLabel]) {
      case (None, childTree) =>
        log(childTree.label.statString)
        Some(childTree.label)
      case (acc @ Some(node), childTree) =>
        val child = childTree.label
        log(child.statString)
        if (node.getAvgReward < child.getAvgReward)
          Some(childTree.label)
        else acc
    }
    result.flatMap { node =>
      log(s"ai spent ${(System.currentTimeMillis() - startTime)}, numSim : ${node.numSim}, ${node.nbWin}/${node.nbLoss},  ${i} iterations")
      node.commandOpt
    }
  }

  def log(s : String) = println(s)

  def defaultPolicy(loc : TreeP) = {
    val tree= loc.tree
    var nbStep = 0
    val node = tree.label
    var state = node.from
    var end = node.end
    var player = node.playerId
    bot.updater.resetStats()
    val cardUsage = new CardStatUsage(cardStats)
    while(nbStep < defaultPolicyMaxTurn && end.isEmpty){
      val nextCommand = choices.getRandomMove(state, player)
      nextCommand.foreach{ c => cardUsage.cardUsed(player, c.card) }
      val (gameState, transition) = bot.simulateCommand(state, player, nextCommand)
      state = gameState
      player = transition.playerId
      nbStep += 1
      end = state.checkEnded
    }
    val reward = updateStats(loc, state, end, bot.updater.stats)
    cardUsage.updateStats(reward)
  }

  def updateStats(loc : TreeP, st : GameState, end : Option[PlayerId], stats : List[PlayerStats]) = {
    val tree = loc.tree
    val node = tree.label
    node.numSim += 1
    val h = heuris(st, node.playerStats, loc.depth)
    val reward = end.map{p => if (p == botPlayerId) {
      node.nbWin += 1
      h
    } else {
      node.nbLoss += 1
      if (h <= 0) h else - 1/ h
    } }.getOrElse(settings.rewardDefaultWeightFactor * h)
    node.rewards += reward

    loc.backPropagate{ t =>
      val node = t.label
      end.foreach{ p =>
        if (p == botPlayerId) { node.nbWin += 1  } else { node.nbLoss += 1 }
      }
      node.numSim += 1
      node.rewards += reward
    }
    reward
  }

  def openAndGetFirstChild(loc : TreeP) : Option[TreeP] = {
    val label = loc.tree.label
    val children = label.open()
    if (children.nonEmpty){
      if (children.forall(_.end.isDefined)){
        label.poisoned = true
      }
      if (loc.tree.subforest.isEmpty){
        val childTrees = children.map { c => new Tree(c) }
        loc.tree.subforest ++= childTrees
      }
      Some(loc.child)
    } else None
  }

  def isDeadEnd(loc : TreeP) = (loc.depth == maxDepth) || loc.tree.label.poisoned

  final def treePolicy(start: TreeP) : Option[TreeP] = {
    var loc = start
    var end = false
    def endWith(t : TreeP){
      loc = t
      end = true
    }

    while (!end) {
      select(loc)
      val depth = loc.depth
      if (depth == maxDepth){
        endWith(loc)
      } else {
        openAndGetFirstChild(loc) match {
          case None =>
            if (loc.hasNext) loc.goNext()
            else {
              loc.parent match {
                case None => endWith(start)
                case Some(p) => loc = p
              }
            }
          case Some(child) => loc = child
        }
      }
    }

    if (loc ne start) Some(loc) else None
  }

  private def select(t : TreeP) = {
    t.parent.foreach{ p  =>
      var best = (t.tree, t.pos)
      val toggled = if (t.tree.label.playerId != botPlayerId) !( _ : Boolean) else identity[Boolean] _
      while(t.hasNext){
        t.goNext()
        if (toggled(t.tree.label.getUct > best._1.label.getUct)) {
          best = (t.tree, t.pos)
        }
      }
      t.goto(best._2)
    }
  }


  case class Node(
    from: GameState,
    transition : Transition,
    playerStats : List[PlayerStats], commandOpt: Option[Command] = None, parent : Option[Node] = None) extends {

    val end = from.checkEnded
    var poisoned = false
    val playerId = transition.playerId

  } with NodeStats {
    private var children = List.empty[Node] // after opened can't be empty if not ended
    final def notExpanded = end.isEmpty && children.isEmpty

    final def open() = {
      if (notExpanded) {
        searchChildren()
      }
      children
    }

    final def searchChildren(){
      val commandChoices = choices.getNexts(from, transition.playerId)
      children = (None :: commandChoices.map(Some(_)).toList).flatMap{ cmd =>
        val (randWidth, n) = getChild(cmd)
        if (randWidth > 1) {
          n :: List.fill(math.max(randWidth, 2)){ getChild(cmd)._2 }
        } else List(n)
      }
    }

    private def getChild(commandOpt: Option[Command]) = {
      val (state, outTransition) = bot.simulateCommand(from, playerId, commandOpt)
      val randWidth = bot.updater.randLogs.width
      bot.updater.resetRand()
      val playerStats = bot.updater.stats.map(_.copy())
      (randWidth, Node(state, outTransition, playerStats, commandOpt, Some(this)))
    }
  }

  trait NodeStats { _ : Node =>

    var numSim = 1
    var nbWin = 0
    var nbLoss = 0
    var rewards = 0f

    end.foreach{ p =>
      if (p == botPlayerId) {
        nbWin += 1
      } else {
        nbLoss += 1
      }
    }

    def getAvgReward : Float = rewards/numSim
    def getUct : Float =  parent.map{ p =>
      (getAvgReward / 5f) + math.sqrt(2 * math.log(p.numSim)/numSim).floatValue  // HACK to try to have reward in [0, 1], it goes rarely up to 10
    }.getOrElse(0f)
    def statString = s"Node($commandOpt : $getUct, avgRwd=$getAvgReward, nSim=$numSim)"
  }

  class CardStatUsage(cardStats : List[CardStats]) {
    var botCards = List.empty[Card]
    var oppCards = List.empty[Card]
    def cardUsed(player: PlayerId, card : Card){
      if (player == human){
        oppCards = card :: oppCards
      } else {
        botCards = card :: botCards
      }
    }
    def updateStats(reward : Float){
      cardStats(botPlayerId).update(reward, botCards)
      cardStats(human).update(reward, oppCards)
    }
  }

}


// for random effects, the simulator add a max of 2 additional evaluations
class BoundedBot2(val botPlayerId: PlayerId, val gameDesc : GameDesc, val spHouses : Houses, val settings : Settings = new Settings) extends Bot {

  def executeAI(start: GameState) = {
    val st = k.ripDescReader(start)
    initGameUpdater(st)
    new BoundedBot2AI(botPlayerId, st, this, settings).execute()
  }
}
