package score


import shared.{Board,Game}


object Result {
  def round(d: Double): Double = {
    (math rint d * 100000) / 100000
  }
}

case class Result(blackWins: Double, whiteWins: Double) {
  override def toString(): String = {
    this match {
      case Result(1, 0) => "Black wins"
      case Result(0, 1) => "White wins"
      case Result(0.5, 0.5) => "Tie game"
      case _ => "Black - White: %.1f - %.1f; avg: %f".format(blackWins, whiteWins, average)
    }
  }

  def record = blackWins - whiteWins

  def average: Double = {
    if (blackWins + whiteWins > 0) {
      Result.round(blackWins / (blackWins + whiteWins).toDouble)
    } else 0
  }

  def +(other: Result): Result = {
    Result(this.blackWins + other.blackWins, this.whiteWins + other.whiteWins)
  }

  def >(other: Result): Boolean = { this.average > other.average }
  def <(other: Result): Boolean = { this.average < other.average }
}


case object TrompTaylor {

  def score(game: Game): Double = {
    // count stones
    val nofBlackStones: Int = game.board.position count (_ == Board.BLACK) 
    val nofWhiteStones: Int = game.board.position count (_ == Board.WHITE) 

    def eval(point: Int): Int = {
      /**
       * return  1 if point has at least one black neighbor and no white neighbor
       * return -1 if point has at least one white neighbor and no black neighbor
       * return  0 otherwise
      **/
      val ns = game.neighbours(point) map (game.board.position(_))
      var result = 0
      if (ns contains Board.BLACK) { result +=  1 }
      if (ns contains Board.WHITE) { result += -1 }
      result
    }

    val territoryScore: Int = (game.board.position filter (_ == Board.EMPTY) map eval).sum

    nofBlackStones - nofWhiteStones + territoryScore
  }
}
