package shared


import scala.language.implicitConversions

import upickle.default._
import upickle.Js


case class BoardSize(x: Int, y: Int) {
  def serialize(): String = { """{"sizeX": %d, "sizeY": %d}""".format(x, y) }
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

  import scala.collection.mutable.{Map => Dict}
  val neighbourMaps = Dict.empty[BoardSize, Map[Int, List[Int]]]

  // memoized function
  // returns neighbours (in coords: "F3" -> List("E3", "F2", "G3", "F4") for a given boardsize
  def universalNeighbours(boardSize: BoardSize): Map[Int, List[Int]] = {
    def f(boardSize: BoardSize): Map[Int, List[Int]] = {
      println("Cache miss: %s".format(boardSize.toString))
      (
        for { i <- 0 until boardSize.x * boardSize.y }
        yield { (i -> Coord(i)(boardSize).neighbours.map(_.toInt)) }
      ).toMap
    }
    neighbourMaps.getOrElseUpdate(boardSize, f(boardSize))
  }

  def clone(board: Board): Board = {
    val copy = new Board(board.boardSize)
    Array.copy(board.position, 0, copy.position, 0, board.position.size) 
    copy.ko = board.ko
    copy
  }
}

class Board(val boardSize: BoardSize) {
  val position = new Array[Int](boardSize.x * boardSize.y)
  val marks = new Array[Int](boardSize.x * boardSize.y)  // do I need marks?
  var ko = Coord.invalid
  lazy val neighbours = Board.universalNeighbours(boardSize)

  def setStone(c: Coord, t: Int) {
    position(c) = t
  }


  def getStone(coord: Coord): Int = {
    if (coord.isValid) position(coord) else Board.UNKNOWN
  }

  def getStone(string: String): Int = { getStone(Coord(string)(boardSize)) }

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
