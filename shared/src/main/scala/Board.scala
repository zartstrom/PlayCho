package shared

import scala.language.implicitConversions


case class BoardSize(x: Int, y: Int) {
  def serialize(): String = { """{"sizeX": %d, "sizeY": %d}""".format(x, y) }
}


class Coord(val x: Int, val y: Int)(implicit val boardSize: BoardSize) {
  def getIsValid(): Boolean = {
    x >= 0 && x < boardSize.x && y >= 0 && y < boardSize.y
  }
  val isValid = getIsValid()

  // implementation for hashCode and equals follows example in http://www.artima.com/pins1ed/object-equality.html
  // additionally, suppose there is only one invalid coord
  override def hashCode = if (this.isValid) 41 * (41 + x) + y else 0
  override def equals(other: Any) = other match {
    // valid Coords have to have matching x and y, invalid Coords are all the same
    case that: Coord => if (this.isValid) this.x == that.x && this.y == that.y else that.isValid == this.isValid
    case _ => false
  }

  def toInt(): Int = {
    // returns canonical 1-dimensional representation of 2-dimensional coord
    if (isValid) { x + boardSize.x * y } else -1
  }

  override def toString(): String = {
    if (isValid) {
      val letter = if (65 + x >= 73) { (65 + x + 1).toChar } else { (65 + x).toChar }
      letter + (boardSize.y - y).toString
    } else {
      "Out of bounds"
    }
  }

  def neighbours(): List[Coord] = {
    if (isValid) {
      List(
        new Coord(x - 1, y), new Coord(x + 1, y),
        new Coord(x, y - 1), new Coord(x, y + 1)
      ).filter(_.isValid)
    } else { List() }
  }
}

object Coord {
  implicit def toInt(c: Coord): Int = {
    c.toInt
  }

  def apply(p: Int)(implicit boardSize: BoardSize): Coord = {
    val cx = p % boardSize.x
    val cy = p / boardSize.x
    new Coord(cx, cy)
  }

  def apply(s: String)(implicit boardSize: BoardSize): Coord = {
    // Take string coordinate (i.e. "M11") and return Coord
    assert(s.size <= 3)
    // 65 = A; 73 = I; I is not used in coords
    val cx = if (s.head.toInt > 73) { s.head.toInt - 65 - 1 } else { s.head.toInt - 65 }
    val cy = boardSize.y - s.tail.toInt
    new Coord(cx, cy)
  }

  val invalid = new Coord(-1, -1)(BoardSize(19, 19)) // actually boardSize does not matter, but uncool to use here :(
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
  val position = new Array[Int](boardSize.x * boardSize.y)
  val marks = new Array[Int](boardSize.x * boardSize.y)
  var ko = Coord.invalid

  def setStone(c: Coord, t: Int) {
    position(c) = t
  }

  def getStone(coord: Coord): Int = {
    if (coord.isValid) position(coord) else Board.UNKNOWN
  }

  def getCoordByPoint(p: Int): Coord = {
    // works also for non-quadratic boards
    val x = p % boardSize.x
    val y = p / boardSize.x
    new Coord(x, y)(boardSize)
  }

  def placeStones(ss: List[String], player: Int) {
    // ss = List("B5", "F2", "S8", "J10")
    // hey, I dont like the variable name "ss", it does not convey meaning.
    // "coords" lies about the fact, that there no instances of Coord
    for(s <- ss) { setStone(Coord(s)(boardSize), player) }
  }
}
