package score


case class Result(blackWins: Int, whiteWins: Int) {
  override def toString(): String = {
    this match {
      case Result(1, 0) => "Black wins"
      case Result(0, 1) => "White wins"
      case _ => "Black - White: %d - %d".format(blackWins, whiteWins)
    }
  }

  def record = blackWins - whiteWins

  def +(other: Result): Result = {
    Result(this.blackWins + other.blackWins, this.whiteWins + other.whiteWins)
  }

  def >(other: Result): Boolean = {
    this.record > other.record
  }
}
