package baduk


import play.api.Logger
import scala.util.Random

import shared.{Game,Move,Pass,Play}


class RandomPlayer(game: Game) {

  def randomMove(): Play = {
    //val moves = game.legalMoves(game.player, game.board.position)
    val plays = game.legalMoves(game.player, game.board.position) :+ Pass

    //if (moves.size > 0) {
    if (plays.size > 0) {
      val rand = Random
      plays(rand.nextInt(plays.size))
    } else {
      Pass
    }
  }

  def play(): Unit = {
    game.make(randomMove())
  }
}
