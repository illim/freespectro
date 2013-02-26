package priv.sp.bot

import priv.sp._
import util.Random.shuffle
import priv.sp.Creature
import priv.sp.PlayerState
import priv.util.Utils

// stupid bot very slow, reason forward and using a stupid heuristic on life ratio
// todo : 
// - maximize fake player move instead of minimizing it lol
// - update the fakeplayer
class DummyBot(val botPlayerId: PlayerId, val game: Game) extends ExtBot {
  
  private val maxDepth = 2

  def executeAI(start: GameState) = {
    val s = System.currentTimeMillis()
    loop(Path(Nil, start)).map { path =>
      println("ai spent " + (System.currentTimeMillis() - s) + " sc: " + path.score + ", depth:" + path.steps.size)
      path.steps.last._1
    }
  }

  def loop(path: Path) = {
    var playerId = botPlayerId
    var paths = walk(playerId, path)
    var i = 0
    while (i < maxDepth && paths.nonEmpty && paths.head.score.killNbStep < 0) {
      playerId = other(playerId)
      paths = paths.flatMap(p => walk(playerId, p))
      i += 1
    }
    if (paths.size > 1 && paths.head.score.isUndecided) {
      paths.reduceLeftOption { (p0, p1) =>
        def lifeRatio(ps: PathScore) = ps.lifeDeltas(botPlayerId) - ps.lifeDeltas(other(botPlayerId))
        List(p0, p1).map { p =>
          val ratio = lifeRatio(p.score)
          (p, ratio)
        }.sortBy(_._2).last._1
      }
    } else paths.headOption
  }

  def walk(playerId: PlayerId, path: Path): List[Path] = {
    val sim = new Simu(playerId, path)
    (List.empty[Path] /: sim.nexts) {
      case (Nil, path) => List(path)
      case (acc @ p0 :: _, p1) =>
        val sc0 = p0.score
        val sc1 = p1.score
        if (sc1 hide sc0) List(p1) else if (sc1.isDead) acc else p1 :: acc
    }
  }

  case class Path(steps: List[(Command, GameState)], start: GameState) {
    def add(command: Command, state: GameState) = copy(steps = (command, state) :: steps)

    lazy val score = {
      val state = steps.head._2
      if (state.players(botPlayerId).life <= 0) PathScore(isDead = true)
      else if (state.players(other(botPlayerId)).life <= 0) PathScore(killNbStep = steps.size)
      else PathScore(lifeDeltas = playerIds.map { playerId =>
        state.players(playerId).life - start.players(playerId).life
      })
    }
  }

  case class PathScore(isDead: Boolean = false, killNbStep: Int = -1, lifeDeltas: List[Int] = Nil) {
    def hide(p: PathScore) = {
      if (isDead) false
      else if (p.isDead) true
      else killNbStep < p.killNbStep
    }
    def isUndecided = lifeDeltas != Nil
  }

  class Simu(playerId: PlayerId, path: Path) {
    val initState = path.steps.headOption.map(_._2) getOrElse path.start
    val commands: Stream[Command] = getCommandChoices(initState, playerId)
    val nexts: Stream[Path] = commands.map { command =>
      path.add(command, simulateCommand(initState, command))
    }
  }
}