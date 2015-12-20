package actors


import akka.actor._
import scala.concurrent.duration._
import scala.collection.immutable.ListMap

import shared.{Board,Coord,Game,Move}
import score.{Result,TrompTaylor}
import baduk.{Heuristics,RandomPlayer}

import main.Global.porter


object EngineActor {
  case class Start(game: Game)
  case class PlayOutResult(move: Move, result: Result)
  case class GiveBestMove(game: Game)
  case class GiveBestMoves(n: Int, player: Int)
}


class EngineActor() extends Actor with ActorLogging {
  import context._

  def receive = active(Map.empty)

  def active(currentResult: Map[Move, Result]): Receive = {
    case EngineActor.Start(game: Game) => {
      log.info("starting engine")
      val legalMoves = game.legalMoves(game.player)

      val emptyResult = (legalMoves map ( move => (move -> Result(0, 0)) )).toMap

      val candidateMoves = if (game.moveNr < game.boardSize.nofPoints / 2) {
        Heuristics.noRim(legalMoves)
      } else { legalMoves }

      for {
        i <- (0 until 70)
        move <- candidateMoves // take 1
      } yield {
        val child = context.actorOf(Props[PlayOutActor])
        child ! PlayOutActor.Start(Game.clone(game), move)
      }

      context become active(emptyResult)

      // send another periodic tick after the specified delay
      //system.scheduler.schedule(1500 millis, self, EngineActor.GiveBestMoves(5, game.player))
      system.scheduler.scheduleOnce(8000 millis, self, EngineActor.GiveBestMoves(5, game.player))
      //system.scheduler.schedule(1500 millis, 1000 millis, self, EngineActor.GiveBestMoves(5, game.player))
    }
    case EngineActor.PlayOutResult(move, result) => {
      // does scalaz have an operator for this?!
      context become active( currentResult + (move -> (currentResult(move) + result)) )
    }
    case EngineActor.GiveBestMove(game) => {
      val resultMoveOption = bestMoves(currentResult, game.player).headOption

      resultMoveOption match {
        case Some(resultMove) => {
          porter ! PorterActor.BestMove(resultMove._1.coord.toString)
        }
        case None => { log.error("No results available. Need to start engine first.") }
      }
    }
    case EngineActor.GiveBestMoves(n, player) => {
      porter ! PorterActor.BestMoves(bestMoves(currentResult, player) take n)
    }
  }

  def bestMoves(currentResult: Map[Move, Result], player: Int): ListMap[Move, Result] = {
    if (player == Board.BLACK) {
      ListMap(currentResult.toSeq.sortWith(_._2 > _._2 ):_*)
    } else { // Board.WHITE
      ListMap(currentResult.toSeq.sortWith(_._2 < _._2 ):_*)
    }
  }
}


object PlayOutActor {
  case class Start(game: Game, move: Move)
}


class PlayOutActor() extends Actor with ActorLogging {

  def receive = {
    case PlayOutActor.Start(game, lastMove) => {
      //log.info("play out game")
      sender ! EngineActor.PlayOutResult(lastMove, playOut(game, lastMove))
      context stop self
    }
  }

  def playOut(game: Game, lastMove: Move): score.Result = {
    // copy the game somehow
    //log.info("First move of variant: %s".format(lastMove))
    game.make(lastMove)
    val player = new RandomPlayer(game)

    //log.info("%d".format(game.moveNr))
    while (game.terminated == false) {
      player.play()
      //log.info("%d".format(game.moveNr))
    }
    val result = evaluate(game)  // Not a good way
    result
  }

  def evaluate(game: Game): score.Result = {
    // set komi to 0.5
    val gameScore = TrompTaylor.score(game)
    log.info("score: %f".format(gameScore))
    if (gameScore > 0) {
      score.Result(1, 0)
    } else if(gameScore < 0) {
      score.Result(0, 1)
    } else {
      score.Result(0.5, 0.5)
    }
  }
}
