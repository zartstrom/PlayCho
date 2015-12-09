package baduk


import play.api.Logger
import scala.util.Random

import shared.{Game,Move,Pass,Play}


class RandomPlayer(game: Game) {

  def randomMove(): Play = {
    val moves = game.legalMoves(game.player, game.board.position)

    if (moves.size > 0) {
      val rand = Random
      moves(rand.nextInt(moves.size))
    } else {
      Pass
    }
  }

  def play(): Play = {

    randomMove() match {
      case move: Move => {
        game.make(move)
        move
      }
      case Pass => Pass
    }
  }

}
