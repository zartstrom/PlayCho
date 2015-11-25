package actors


import akka.actor._
import scala.concurrent.duration._
import scala.util.Random

import shared.{Board,Coord,Game,Move}

import main.Global.porter
import messages.Simple


class EngineActor() extends Actor with ActorLogging {
  import context._
  var loop = 0

  def receive = {
    case Msg.StartEngine(game: Game) => {
      log.info("starting engine")
      // send another periodic tick after the specified delay
      loop = 0
      system.scheduler.scheduleOnce(1000 millis, self, EngineActor.GiveBestMove(game))
    }
    case Msg.GiveBestMove(game) => {
      val coord: Coord = game.randomMove(Board.BLACK)
      log.info("give best move %s".format(coord.toString))

      porter ! Msg.CurrentBestMove(coord.toString)
      // loop
      loop += 1
      if (loop < 3) { system.scheduler.scheduleOnce(1000 millis, self, Msg.GiveBestMove(game)) }
    }
  }
}


class PlayOutActor() extends Actor with ActorLogging {
  
  def receive = {
    case PlayOut(game, lastMove) => {
      sender ! Msg.Result(playOut(game))
    }

  def playOut(game, lastMove): score.Result = {
    game.make(lastMove)(game.board.position)

    val maxMoves: Int = game.board.boardSize.x * game.board.boardSize.y * 80 / 100

    def helper: score.Result = {
      if (game.moveNr < maxMoves) {
        game.makeRandomMove()
        helper
      } else {
        evaluate(game)  // Not a good way
      }
    }

    helper
  }

  def evaluate(game): score.Result = {
    // set komi to 0.5
    if (game.score > 0) {
      Result(1, 0)
    } else {
      Result(0, 1)
    }
  }
}
