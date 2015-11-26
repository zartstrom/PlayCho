package score


case class Result(blackWins: Int, whiteWins: Int) {
  override def toString(): String = {
    this match {
      case Result(1, 0) => "Black wins"
      case Result(0, 1) => "White wins"
      case _ => "Black - White: %d - %d".format(blackWins, whiteWins)
    }
  }
}
