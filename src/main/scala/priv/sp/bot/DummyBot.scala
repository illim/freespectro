package priv.sp.bot

import priv.sp._
import util.Random.shuffle
import priv.sp.Creature
import priv.sp.PlayerState
import scala.util.continuations._
import priv.util.Utils

// stupid bot very slow and using a stupid heuristic on life ratio
class DummyBot(botPlayerId: PlayerId, game: Game) extends Bot {

  // todo update the fakeplayer
  private val fakePlayer = CardShuffle.createOnePlayer(CardShuffle.filterHouses(game.state.players(botPlayerId))(CardShuffle.baseHouses))
  private val maxDepth = 2

  def executeAI(start: GameState) = shift { k: (Option[Command] => Unit) =>
    val ripped = (game.playersLs(other(botPlayerId)).replaceCards(fakePlayer.houses) run start)._1
    Utils.threaded {
      val s = System.currentTimeMillis()
      k(loop(Path(Nil, ripped)).map { path =>
        println("ai spent " + (System.currentTimeMillis() - s) + " sc: " + path.score + ", depth:" + path.steps.size)
        path.steps.last._1
      })
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

  def getHouses(state: GameState, playerId: PlayerId) = {
    if (playerId == botPlayerId) {
      state.players(playerId).houses
    } else {
      fakePlayer.houses
    }
  }

  class Simu(playerId: PlayerId, path: Path) {
    val initState = path.steps.headOption.map(_._2) getOrElse path.start
    val cards = getHouses(initState, playerId).flatMap { house => house.cards.filter(_.isAvailable(house)) }

    val commands: Stream[Command] = {
      cards.toStream.flatMap { card =>
        card.inputSpec match {
          case None => List(Command(playerId, card, Nil))
          case Some(SelectOwnerSlot) =>
            slotRange.filterNot(s => initState.players(playerId).slots.exists(_._1 == s)).map { num =>
              Command(playerId, card, List(OwnerSlot(num)))
            }
          case Some(SelectOwnerCreature) => Nil
          case Some(SelectTargetCreature) => Nil
        }
      }
    }

    val nexts: Stream[Path] = commands.map { command =>
      val commandState = (game.getCommandEffect(command) run initState)._1
      val runState = (commandState /: commandState.players(playerId).slots) {
        case (st, (numSlot, slot)) =>
          (game.runSlot(playerId, numSlot, slot) run st)._1
      }
      val nextState = (game.prepareNextTurn(other(playerId)) run runState)._1
      path.add(command, nextState)
    }
  }
}