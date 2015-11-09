package board


object Board {

  val EMPTY = 0
  val BLACK = 1
  val WHITE = 2
  val SEMI_BLACK = 3
  val SEMI_WHITE = 4
  val MIXED = -1
  val UNKNOWN = -2

}

class Board(val boardSize: BoardSize) {
  val stones = new Array[Int](boardSize.x * boardSize.y)
  val marks = new Array[Int](boardSize.x * boardSize.y)

  def setStone(c: Coordinate, t: Int) {
    stones(c.toIndex) = t
  }

  def getStone(c: Coordinate): Int = {
    if (c.isValid) stones(c.toIndex) else Board.UNKNOWN
  }

  def getCoordinateByPoint(p: Int): Coordinate = {
    // works also for non-quadratic boards
    val x = p % boardSize.x
    val y = p / boardSize.x
    new Coordinate(x, y)(boardSize)
  }
}
