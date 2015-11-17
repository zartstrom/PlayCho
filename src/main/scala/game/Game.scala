package baduk

import collection.mutable.ArrayBuffer
import collection.mutable.HashMap
import math.abs
import scala.util.Random
import scala.util.{Try, Success, Failure}


case class Move(coord: Coord, player: Int)


object Game {

  // need these here?
  val EMPTY = 0
  val BLACK = 1
  val WHITE = 2
  val MIXED = -1

  def apply(boardSize: BoardSize): Game = {
    val board = new Board(boardSize)
    new Game(board)
  }
  
  def apply(size: Int): Game = {
    val boardSize = BoardSize(size, size)
    apply(boardSize)
  }
}

class Game(val board: Board, val komi: Double = 6.5) {
  // separated Board and Game classes; lets see how this plays out
  implicit val boardSize = board.boardSize

  var moveNr = 0
  var capturedStonesBy = HashMap(Board.BLACK -> 0, Board.WHITE -> 0)

  // keep it for later!
  //val displayer = new TerminalDisplay(this.size)
  //def display { this.displayer.display(board.stones) }

  val neighbours: Map[Int, List[Int]] = (for { 
    i <- 0 until board.stones.size
  } yield { (i -> Coord(i).neighbours.map(_.toInt)) }).toMap

  def makeRandomMove(color: Int): Int = {
    val lm = this.legalMoves(board.stones, color)
    val r = Random
    val move = lm(r.nextInt(lm.size))
    this.make(move)
    move.coord
  }

  def placeStone(points: Array[Int], move: Move) {
    points(move.coord) = move.player
  }

  def getStone(point: Int): Int = {
    board.stones(point)
  }

  def getStone(s: String): Int = {
    getStone(Coord(s))
  }

  // rename one of the placeStone / placeStones methods, because signatures are too different

  def placeStones(points: Array[Int], coords: List[String], player: Int) {
    for(s <- coords) { placeStone(points, Move(Coord(s), player)) }
  }

  def isCapture(points: Array[Int], move: Move): Boolean = {
    val enemies = neighbours(move.coord) filter ( x => { val p = points(x);  p != move.player && p != Board.EMPTY } )
    //println("enemies: %s".format(enemies))
    val liberties = enemies map ( getLiberties(points, _).size )
    //println("liberties: %s".format(liberties))
    val killedNeighbors = liberties filter ( _ == 1 )

    killedNeighbors.size > 0
  }

  def isSuicide(points: Array[Int], move: Move): Boolean = {
    val isCapture: Boolean = this.isCapture(points, move)
    //println("isCapture: %s".format(isCapture.toString))

    if (isCapture) { false } else {
      val peek = points.clone
      placeStone(peek, move)
      getLiberties(peek, move.coord).size == 0
    }
  }

  def isEmptyPoint(points: Array[Int], move: Move): Try[Boolean] = {
    if (points(move.coord) != Board.BLACK && points(move.coord) != Board.WHITE) {
      Success(true)
    } else Failure(new Exception("Place the stone on a empty point on the board"))
  }

  // make this snippet smarter
  def isNotSuicide(points: Array[Int], move: Move): Try[Boolean] = {
    val isCapture: Boolean = this.isCapture(points, move)
    //println("isCapture: %s".format(isCapture.toString))

    if (isCapture) { Success(true) } else {
      val peek = points.clone
      placeStone(peek, move)
      if (getLiberties(peek, move.coord).size == 0) { 
        Failure(new Exception("Suicide is not allowed"))
      } else Success(true)
    }
  }

  def isNotKo(koCoord: Coord, move: Move): Try[Boolean] = {
    if (koCoord != move.coord) { Success(true) } else Failure(new Exception("Cannot retake ko immediately"))
  }

  def check(points: Array[Int], move: Move): Try[Move] = {
    for {
      _ <- isEmptyPoint(points, move)
      _ <- isNotSuicide(points, move)
      _ <- isNotKo(Coord("A3"), move) // fix this
    } yield move
  }

  def check(move: Move): Try[Move] = { check(board.stones, move) }

  def removeCapturedStones(points: Array[Int], move: Move): Unit = {
    val enemies = neighbours(move.coord) filter ( x => { val p = points(x);  p != move.player && p != Board.EMPTY } )
    println("enemies: %s".format(enemies))
    // there are possible duplicates, if two or more neighbours are connected
    //val stones = enemies filter ( getLiberties(_).size == 1 ) flatMap (p => connComp(p))
    val test = enemies filter ( getLiberties(points, _).size == 1 )
    println("test: %s".format(test))

    val stones = (enemies filter ( getLiberties(points, _).size == 1 ) flatMap connComp).distinct
    println("stones: %s".format(stones))
    capturedStonesBy(move.player) = capturedStonesBy(move.player) + stones.size
    for(p <- stones) yield { points(p) = Board.EMPTY }
  }

  def removeGroup(point: Int) {
    // too complicated; remove conversion to Move
    for(p <- connComp(point)) yield { placeStone(board.stones, Move(Coord(p), Board.EMPTY)) }
  }

  def make(points: Array[Int], move: Move) {
    // Assume check for validity took place if needed.
    // Otherwise illegal positions may occur, which may be alright when setting up a position
    val isCapture: Boolean = this.isCapture(points, move)
    if (isCapture) { 
      println("try to remove some stones")
      removeCapturedStones(points, move)
    }

    placeStone(points, move)
    this.moveNr += 1
  }

  def make(move: Move) {
    make(board.stones, move: Move)
  }

  def makeMs(ms: List[Move]) {
    for(m <- ms) yield { make(m) }
  }

  def legalMoves(points: Array[Int], player: Int): IndexedSeq[Move] = {
    for {
      p <- 0 until points.size
      move <- check(Move(Coord(p), player)).toOption
    } yield move
  }

  def connComp(points: Array[Int], point: Int): List[Int] = {
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

  def connComp(point: Int): List[Int] = {
    connComp(board.stones, point)
  }

  def getLiberties(points: Array[Int], point: Int): List[Int] = {
    // have to check for BLACK and WHITE positively
    // check for not EMPTY fails by hover stones SEMI_BLACK, SEMI_WHITE
    // thats quite stupid; broken by design, etc..
    def isLiberty(p: Int): Boolean = {
      points(p) != Board.WHITE && points(p) != Board.BLACK
    }

    { connComp(points, point) flatMap ( neighbours(_) filter isLiberty ) }.distinct
  }

  def getLiberties(point: Int): List[Int] = {
    getLiberties(board.stones, point)
  }

  def getLiberties(s: String): List[Int] = {
    // A1 -> `liberties`
    getLiberties(board.stones, Coord(s))
  }

  def connComps(color: Int = Board.EMPTY): List[IndexedSeq[Int]] = {
    // getConnectedComponents
    // inspired by example in https://en.wikipedia.org/wiki/Connected-component_labeling

    var nextLabel = 1
    val disjointSet = new DisjointSet[Int]

    val labels = new Array[Int](board.stones.size)

    // first pass
    for(p <- 0 until board.stones.size if board.stones(p) == color) {
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
    if (points.forall(p => board.stones(p) == Board.BLACK)) {
      Board.BLACK
    } else if (points.forall(p => board.stones(p) == Board.WHITE)) {
      Board.WHITE
    } else {
      Board.MIXED
    }
  }

  def getCleanTerritory(): Map[Int, Int] = {
    val counter = HashMap[Int, Int](Board.BLACK -> 0, Board.WHITE -> 0)

    for {
      cc <- connComps(Board.EMPTY)
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

    // first pass
    for(p <- 0 until board.stones.size if board.stones(p) == color) {
      disjointSet += p
      val ps = (p :: neighbours(p)) filter (x => board.stones(x) == color && x <= p)
      for(l <- ps; k <- ps  if k > l) { disjointSet.union(l, k) }
    }
    disjointSet
  }

  def groupLiberties(cc: List[Int]): List[Int] = {
    val tmp = for(p <- cc; n <- neighbours(p) if board.stones(n) == Board.EMPTY) yield n
    tmp.distinct
  }

  def getTerritories(point: Int): List[Iterable[Int]] = {
    val cc = connComp(point)

    val dset = getDisjointSet(board.stones, Board.EMPTY)

    val keys = for(liberty <- groupLiberties(cc)) yield { dset(liberty) }

    keys.distinct map (x => dset.set(x))
  }

  def getTerritories(s: String): List[Iterable[Int]] = {
    getTerritories(Coord(s))
  }
}

