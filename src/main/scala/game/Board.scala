package game


case class BoardSize(x: Int, y: Int)


class Coordinate(val x: Int, val y: Int)(implicit boardSize: BoardSize) {
  def getIsValid(): Boolean = {
    x >= 0 && x < boardSize.x && y >= 0 && y < boardSize.y
  }
  val isValid = getIsValid()

  // implementation for hashCode and equals follows example in http://www.artima.com/pins1ed/object-equality.html
  // act if there is only one invalid coordinate
  override def hashCode = if (this.isValid) 41 * (41 + x) + y else 0
  override def equals(other: Any) = other match { 
    // valid Coordinates have to have matching x and y, invalid Coordinates are all the same
    case that: Coordinate => if (this.isValid) this.x == that.x && this.y == that.y else that.isValid == this.isValid
    case _ => false 
  }

  def toIndex(): Int = {
    // return canonical 1-dimensional representation of 2-dimensional coordinate
    if (isValid) x + boardSize.x * y else -1
  }

  override def toString(): String = {
    if (isValid) {
      val letter = if (65 + x >= 73) { (65 + x + 1).toChar } else { (65 + x).toChar }
      letter + (boardSize.y - y).toString
    } else {
      "Out of bounds"
    }
  }
}

object Board {

  // this are stones, players, or something else. Get it straight
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
