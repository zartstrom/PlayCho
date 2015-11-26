package actors


import akka.actor._
import scala.concurrent.duration._
import scala.util.Random

import shared.{Board,Coord,Game,Move}

import main.Global.porter


class EngineActor() extends Actor with ActorLogging {
  import context._
  var loop = 0

  def receive = {
    case Msg.StartEngine(game: Game) => {
      log.info("starting engine")
      // send another periodic tick after the specified delay
      loop = 0
      system.scheduler.scheduleOnce(1000 millis, self, Msg.GiveBestMove(game))
    }
    case Msg.GiveBestMove(game) => {
      val move: Move = game.randomMove(Board.BLACK)
      log.info("give best move %s".format(move.coord.toString))

      val child = context.actorOf(Props[PlayOutActor])
      child ! Msg.PlayOut(game, move)

      porter ! Msg.CurrentBestMove(move.coord.toString)
      // loop
      loop += 1
      if (loop < 3) { system.scheduler.scheduleOnce(1000 millis, self, Msg.GiveBestMove(game)) }
    }
  }
}


class PlayOutActor() extends Actor with ActorLogging {
  
  def receive = {
    case Msg.PlayOut(game, lastMove) => {
      log.info("play out game")
      sender ! Msg.PlayOutResult(playOut(game, lastMove))
    }
  }

  def playOut(game: Game, lastMove: Move): score.Result = {
    game.make(lastMove)(game.board.position)

    // determine the number of moves to play out. Later need a game termination criteria
    val nofMoves: Int = 70 - game.moveNr

    def helper(nofMoves: Int): score.Result = {
      if (nofMoves > 0) {
        log.info("player: %d, moveNr: %d".format(game.player, game.moveNr))
        game.randomMove(game.player)
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
    log.info("score: %f".format(game.score))
    if (game.score > 0) {
      score.Result(1, 0)
    } else {
      score.Result(0, 1)
    }
  }
}
