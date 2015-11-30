package shared


import scala.language.implicitConversions

import upickle.default._
import upickle.Js


class Coord(val x: Int, val y: Int)(implicit val boardSize: BoardSize) {
  val isValid: Boolean = {
    x >= 0 && x < boardSize.x && y >= 0 && y < boardSize.y
  }

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

  // custom pickler - see http://lihaoyi.github.io/upickle-pprint/upickle/#CustomPicklers
  implicit val coord2Writer = upickle.default.Writer[Coord]{
    case c: Coord => Js.Obj(
      ("string" -> Js.Str(c.toString)),
      ("x" -> Js.Num(c.boardSize.x)),
      ("y" -> Js.Num(c.boardSize.y))
    )
  }
}
