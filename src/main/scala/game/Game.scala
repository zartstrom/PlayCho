package game

import collection.mutable.ArrayBuffer
import collection.mutable.HashMap
import java.security.MessageDigest
import math.abs
import scala.util.Random


case class M(color: Int, point: Int)
case class Move(color: Int, coord: String)
case class CoolMove(c: Coordinate, player: Int)


object Game {

  val EMPTY = 0
  val BLACK = 1
  val WHITE = 2
  val MIXED = -1

  val digest = MessageDigest.getInstance("MD5")

  def getCoordinate(size: Int, point: Int): String = {
    val letter = {
      val mod = point % size
      if (65 + mod >= 73) { (65 + mod + 1).toChar } else { (65 + mod).toChar }
    }
    letter + (size - point / size).toString
  }

  def getPoint(size: Int, coordinate: String): Int = {
    assert(coordinate.size <= 3)
    // 65 = A; 73 = I; I is not used in coordinates
    val col = if (coordinate.head.toInt > 73) { coordinate.head.toInt - 65 - 1 } else { coordinate.head.toInt - 65 }
    val row = size - coordinate.tail.toInt

    col + size * row
  }

  def getPointCoordMap(size: Int): Map[Int, String] = {
    (0 until size * size) map ( x => Map(x -> getCoordinate(size, x)) ) reduce (_ ++ _)
  }

  def getCoordPointMap(size: Int): Map[String, Int] = {
    getPointCoordMap(size) map (_.swap)
  }

  def hashBoard(points: Array[Int]): String = {
    digest.digest(points map (_.toByte)).map("%02x".format(_)).mkString
  }

}

class Game(val board: Board, val komi: Double = 6.5) {
  // separated Board and Game classes; lets see how this plays out
  val sizeX = board.boardSize.x
  val sizeY = board.boardSize.y
  val points = new Array[Int](sizeX * sizeY)

  var moveNr = 0
  var seen = Map[String, Int]()
  var capturedStonesBy = HashMap(Board.BLACK -> 0, Board.WHITE -> 0)
  //val displayer = new TerminalDisplay(this.size)

  //def display {
  //  this.displayer.display(this.points)
  //}

  val coordPointMap = Game.getCoordPointMap(this.sizeX) // TODO: non-quadratic
  val pointCoordMap = Game.getPointCoordMap(this.sizeX) // TODO: non-quadratic

  def getNeighbours(point: Int): List[Int] = {
    val potentialNeighbours = List(point - this.sizeY, point + this.sizeY, point - 1, point + 1)

    def good(p: Int): Boolean = {
      p >= 0 && p < this.points.size && { abs(p % this.sizeX - point % this.sizeX) <= 1 }
    }

    potentialNeighbours filter good
  }

  var neighbours = new HashMap[Int, List[Int]]

  def initNeighbourHash {
    for(i <- 0 until this.points.size) {
      neighbours(i) = this.getNeighbours(i)
    }
  }
  initNeighbourHash

  def makeRandomMove(color: Int): Int = {
    val lm = this.legalMoves(color)
    val r = Random
    val move = lm(r.nextInt(lm.size))
    this.make(move)
    //this.getCoordinate(point)
    move.point
  }

  def convert(move: Move): M = {
    M(move.color, coordPointMap(move.coord))
  }

  def placeStone(points: Array[Int], m: M) {
    points(m.point) = m.color
  }

  def getStone(point: Int): Int = {
    points(point)
  }

  def getStone(coord: String): Int = {
    getStone(coordPointMap(coord))
  }

  // rename one of the placeStone / placeStones methods, because signatures are too different

  def placeStones(color: Int, coords: List[String]) {
    for(c <- coords) { make(Move(color, c)) }
  }

  def isCapture(m: M): Boolean = {
    val enemies = neighbours(m.point) filter ( x => { val p = this.points(x);  p != m.color && p != Board.EMPTY } )
    val liberties = enemies map ( getLiberties(_).size )
    val killed = liberties filter ( _ == 1 )

    killed.size > 0
  }

  def isCapture(move: Move): Boolean = {
    isCapture(convert(move))
  }

  def removeCapturedStones(points: Array[Int], m: M) {
    val enemies = neighbours(m.point) filter ( x => { val p = points(x);  p != m.color && p != Board.EMPTY } )
    // there are possible duplicates, if two or more neighbours are connected
    //val stones = enemies filter ( getLiberties(_).size == 1 ) flatMap (p => getCC(p))
    val stones = (enemies filter ( getLiberties(_).size == 1 ) flatMap getCC).toSet.toList
    capturedStonesBy(m.color) = capturedStonesBy(m.color) + stones.size
    for(p <- stones) yield { points(p) = Board.EMPTY }
  }

  def removeGroup(point: Int) {
    for(p <- getCC(point)) yield { placeStone(this.points, M(p, Board.EMPTY)) }
  }

  def isSuicide(m: M): Boolean = {
    val isCapture: Boolean = this.isCapture(m)

    if (isCapture) { false } else {
      val b = this.points.clone
      placeStone(b, m)
      getLiberties(b, m.point).size == 0
    }
  }

  def isSuicide(move: Move): Boolean = {
    isSuicide(convert(move))
  }

  def isLegal(points: Array[Int], m: M): Boolean = {
    // clunky stuff
    try {
      assert (points(m.point) == Board.EMPTY, "place stone on empty point")
      assert (!this.isSuicide(m), "do not commit suicide")
      // check for suicide
      // check for capture
      val isCapture: Boolean = this.isCapture(m)
      if (isCapture) {
        val b = points.clone
        removeCapturedStones(b, m)
        placeStone(b, m)
        val hash = Game.hashBoard(b)
        assert (!this.seen.contains(hash) || this.seen(hash) - this.moveNr > 2)
      }
      true

    } catch {
      case _: AssertionError => false
    }
  }

  def isLegal(move: Move): Boolean = {
    isLegal(this.points, convert(move))
  }

  def isLegal(cm: CoolMove): Boolean = {
    isLegal(this.board.stones, M(cm.player, cm.c.toIndex))
  }

  def make(points: Array[Int], m: M) {
    // case class M(color: Int, point: Int)
    // check for legality
    val isCapture: Boolean = this.isCapture(m)
    if (isCapture) { removeCapturedStones(points, m) }

    placeStone(points, m)
    this.seen = this.seen ++ Map(Game.hashBoard(points) -> this.moveNr)
    this.moveNr += 1
  }

  def make(m: M) {
    make(this.points, m: M)
  }

  def make(move: Move) {
    // case class Move(color: Int, coord: String)
    // the 'public' method for making moves, like make(Move(BLACK, "D5"))
    // check for validity
    val m = M(move.color, coordPointMap(move.coord))
    this.make(m)
  }

  def makeMoves(moves: List[Move]) {
    val ms  = moves map ( x => M(x.color, coordPointMap(x.coord)) )
    makeMs(ms)
  }

  def makeMs(ms: List[M]) {
    for(m <- ms) yield { make(m) }
  }

  def legalMoves(color: Int): IndexedSeq[M] = {
    // take care of no-suicide and ko rules later
    val candidates = for(p <- 0 until this.points.size if this.points(p) == Board.EMPTY) yield M(color, p)
    candidates filter ( m => !isSuicide(m) )
  }

  def getCC(points: Array[Int], point: Int): List[Int] = {
    // get connected component of stones or territory
    val color = points(point)

    val stream = Stream[Int](point)
    val seen = Set[Int](point)
    val result = List()

    def helper(stream: Stream[Int], seen: Set[Int], result: List[Int]): List[Int] = {
      if (stream.isEmpty)
        result
      else {
        val p = stream.head
        val nbs = neighbours(p) filter (points(_) == color)
        helper(
          stream.tail ++ ( nbs filter ( !seen.contains(_) ) ),
          seen ++ nbs,
          p :: result
        )
      }
    }

    helper(stream, seen, result)
  }

  def getCC(point: Int): List[Int] = {
    getCC(this.points, point)
  }

  def getLiberties(points: Array[Int], point: Int): List[Int] = {
    val tmp = getCC(points, point) flatMap (getNeighbours(_) filter (x => points(x) == Board.EMPTY))
    tmp.toSet.toList
  }

  def getLiberties(point: Int): List[Int] = {
    getLiberties(this.points, point)
  }

  def getLiberties(coord: String): List[Int] = {
    val point = coordPointMap(coord)
    getLiberties(point)
  }

  def getCCs(color: Int = Board.EMPTY): List[IndexedSeq[Int]] = {
    // getConnectedComponents
    // inspired by example in https://en.wikipedia.org/wiki/Connected-component_labeling

    var nextLabel = 1
    val disjointSet = new DisjointSet[Int]

    val labels = new Array[Int](this.points.size)

    // first pass
    for(p <- 0 until this.points.size if this.points(p) == color) {
      val ns = neighbours(p) filter (x => labels(x) != 0)
      if (ns.size == 0) {
        disjointSet += nextLabel
        labels(p) = nextLabel
        nextLabel += 1
      } else {
        val localLabels = ns map (labels(_))
        labels(p) = localLabels.min
        for(l <- localLabels; k <- localLabels if l < k) { disjointSet.union(l, k) }
      }
    }

    // second pass
    for(i <- 0 until labels.size if labels(i) != 0) { labels(i) = disjointSet(labels(i)) }

    val components = (0 until labels.size filter (labels(_) != 0) groupBy (labels(_))).values
    components.toList

    //val result = components map (li => (li map (pointCoordMap(_))))
    //result
  }

  def getRim(connectedComponent: Iterable[Int]): IndexedSeq[Int] = {
    val ourFamily = connectedComponent.toSet
    val rs = for(c <- connectedComponent; n <- neighbours(c) if !ourFamily.contains(n)) yield n
    // remove duplicates
    rs.toArray.distinct // improve on this collections mess
  }

  def cleanColor(points: IndexedSeq[Int]): Int = {
    if (points.forall(p => this.points(p) == Board.BLACK)) {
      Board.BLACK
    } else if (points.forall(p => this.points(p) == Board.WHITE)) {
      Board.WHITE
    } else {
      Board.MIXED
    }
  }

  def getCleanTerritory(): Map[Int, Int] = {
    val counter = HashMap[Int, Int](Board.BLACK -> 0, Board.WHITE -> 0)

    for {
      cc <- getCCs(Board.EMPTY)
      color = cleanColor(getRim(cc))
      if Set(Board.BLACK, Board.WHITE).contains(color)
    } yield { counter(color) += cc.size }

    counter.toMap
  }

  def getScore(): Double = {
    val territory = getCleanTerritory()

    val blackTerritory = territory(Board.BLACK)
    val whiteTerritory = territory(Board.WHITE)

    val blackScore = blackTerritory + capturedStonesBy(Board.BLACK)
    val whiteScore = whiteTerritory + capturedStonesBy(Board.WHITE) + this.komi

    blackScore - whiteScore
  }

  def getDisjointSet(points: Array[Int], color: Int): DisjointSet[Int] = {
    // inspired by example in https://en.wikipedia.org/wiki/Connected-component_labeling
    val disjointSet = new DisjointSet[Int]
    //val labels = new Array[Int](this.points.size)
    //var nextLabel = 1

    // first pass
    for(p <- 0 until this.points.size if this.points(p) == color) {
      disjointSet += p
      val ps = (p :: neighbours(p)) filter (x => this.points(x) == color && x <= p)
      for(l <- ps; k <- ps  if k > l) { disjointSet.union(l, k) }
    }
    disjointSet
  }

  def groupLiberties(cc: List[Int]): List[Int] = {
    val tmp = for(p <- cc; n <- neighbours(p) if this.points(n) == Board.EMPTY) yield n
    tmp.distinct
  }

  def getTerritories(point: Int): List[Iterable[Int]] = {
    val cc = getCC(point)

    val dset = getDisjointSet(this.points, Board.EMPTY)

    val keys = for(liberty <- groupLiberties(cc)) yield { dset(liberty) }

    keys.distinct map (x => dset.set(x))
  }

  def getTerritories(coord: String): List[Iterable[Int]] = {
    getTerritories(coordPointMap(coord))
  }
}

