package actors


import akka.actor._
import scala.concurrent.duration._
import scala.util.Random
import scala.collection.immutable.ListMap

import shared.{Board,Coord,Game,Move}
import score.{Result,TrompTaylor}

import main.Global.porter


class EngineActor() extends Actor with ActorLogging {
  import context._

  def receive = active(Map.empty)

  def active(currentResult: Map[Move, Result]): Receive = {
    case Msg.StartEngine(game: Game) => {
      log.info("starting engine")

      val emptyResult = (game.legalMoves(game.player) map ( move => (move -> Result(0, 0)) )).toMap


      for {
        move <- game.legalMoves(game.player) //take 1
        i <- (0 until 100)
      } yield {
        val child = context.actorOf(Props[PlayOutActor])
        child ! Msg.PlayOut(Game.clone(game), move)
      }

      context become active(emptyResult)

      // send another periodic tick after the specified delay
      system.scheduler.scheduleOnce(8000 millis, self, Msg.GiveBestMove(game))
    }
    case Msg.PlayedOutResult(move, result) => {
      // does scalaz have an operator for this?!
      context become active( currentResult + (move -> (currentResult(move) + result)) )
    }
    case Msg.GiveBestMove(game) => {

      val bestMoves = if (game.player == Board.BLACK) {
        ListMap(currentResult.toSeq.sortWith(_._2 > _._2 ):_*)
      } else { // Board.WHITE
        ListMap(currentResult.toSeq.sortWith(_._2 < _._2 ):_*)
      }

      log.info("Result: %s".format(bestMoves take 5))

      val bestMove = bestMoves.head._1

      porter ! Msg.CurrentBestMove(bestMove.coord.toString)
    }

      // TODO: make this nice
      //game.randomMove(Board.BLACK) match {
      //  case Some(move) => {
      //    log.info("give best move %s".format(move.coord.toString))
      //    val child = context.actorOf(Props[PlayOutActor])
      //    child ! Msg.PlayOut(Game.clone(game), move)
      //    porter ! Msg.CurrentBestMove(move.coord.toString)
      //  }
      //  case None => {
      //    log.info("No move available")
      //  }
      //}
  }
}


class PlayOutActor() extends Actor with ActorLogging {

  def receive = {
    case Msg.PlayOut(game, lastMove) => {
      log.info("play out game")
      sender ! Msg.PlayedOutResult(lastMove, playOut(game, lastMove))
      context stop self
    }
  }

  def playOut(game: Game, lastMove: Move): score.Result = {
    // copy the game somehow
    log.info("First move of variant: %s".format(lastMove))
    game.make(lastMove)

    // determine the number of moves to play out. Later need a game termination criteria
    val nofMoves: Int = 19 - game.nextMoveNr

    def helper(nofMoves: Int): score.Result = {
      if (nofMoves > 0) {
        val moveOpt = game.makeRandomMove(game.player)
        //log.debug(moveOpt.get.toString)
        helper(nofMoves - 1)
      } else {
        val result = evaluate(game)  // Not a good way
        log.info(result.toString)
        result
      }
    }

    helper(nofMoves)
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
