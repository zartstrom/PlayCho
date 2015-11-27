package shared

import collection.mutable.ArrayBuffer
import collection.mutable.HashMap
import math.abs
import scala.util.Random
import scala.util.{Try, Success, Failure}

// TODO: Move it somewhere else
//import org.json4s._
//import org.json4s.native.JsonMethods._
//import org.json4s.JsonDSL._


case class Move(coord: Coord, player: Int) {
  // TODO: do this somewhere else / more cleanly
  def serialize(): String = {
    //val json = ("coord" -> coord.toString) ~ ("player" -> player.toString)
    //compact(render(json))
    """
    {"coord": "%s", "sizeX": %d, "sizeY": %d, "player": %d}
    """.format(coord.toString, coord.boardSize.x, coord.boardSize.y, player)
    // yeah, this has no future - need to find good json library
  }
}


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

  def opponent(player: Int): Int = {
    player match { case `BLACK` => WHITE; case `WHITE` => BLACK; case _ => player }
  }
}

class Game(val board: Board, val komi: Double = 0.5, var player: Int = Board.BLACK) {
  // separated Board and Game classes; lets see how this plays out
  implicit val boardSize = board.boardSize

  var moveNr = 0
  var nofCaptured = HashMap(Board.BLACK -> 0, Board.WHITE -> 0)

  // keep it for later!
  //val displayer = new TerminalDisplay(this.size)
  //def display { this.displayer.display(board.position) }

  val neighbours: Map[Int, List[Int]] = (for {
    i <- 0 until board.position.size
  } yield { (i -> Coord(i).neighbours.map(_.toInt)) }).toMap

  def placeStone(move: Move)(position: Array[Int]) {
    position(move.coord) = move.player
  }

  def getStone(point: Int): Int = { board.position(point) }

  def getStone(s: String): Int = { getStone(Coord(s)) }

  // rename one of the placeStone / placeStones methods, because signatures are too different

  //def placeStones(coords: List[String], player: Int)(position: Array[Int]) {
  //  for(s <- coords) { placeStone(Move(Coord(s), player))(position) }
  //}

  def placeStones(coords: List[Int], player: Int)(position: Array[Int]) {
    for(p <- coords) { placeStone(Move(Coord(p), player))(position) }
  }

  def isCapture(move: Move)(position: Array[Int]): Boolean = {
    val enemies = neighbours(move.coord) filter ( x => { val p = position(x);  p != move.player && p != Board.EMPTY } )
    //println("enemies: %s".format(enemies))
    val liberties = enemies map ( getLiberties(_)(position).size )
    //println("liberties: %s".format(liberties))
    val killedNeighbors = liberties filter ( _ == 1 )

    killedNeighbors.size > 0
  }

  def isSuicide(move: Move)(position: Array[Int]): Boolean = {
    val isCapture: Boolean = this.isCapture(move)(position)
    //println("isCapture: %s".format(isCapture.toString))

    if (isCapture) { false } else {
      val peek = position.clone
      placeStone(move)(peek)
      getLiberties(move.coord)(peek).size == 0
    }
  }

  def isEmptyPoint(move: Move)(position: Array[Int]): Try[Boolean] = {
    if (position(move.coord) != Board.BLACK && position(move.coord) != Board.WHITE) {
      Success(true)
    } else Failure(new Exception("Place the stone on a empty point on the board"))
  }

  // make this snippet smarter
  def isNotSuicide(move: Move)(position: Array[Int]): Try[Boolean] = {
    val isCapture: Boolean = this.isCapture(move)(position)
    //println("isCapture: %s".format(isCapture.toString))

    if (isCapture) { Success(true) } else {
      val peek = position.clone
      placeStone(move)(peek)
      if (getLiberties(move.coord)(peek).size == 0) {
        Failure(new Exception("Suicide is not allowed"))
      } else Success(true)
    }
  }

  def isNotKo(move: Move, koCoord: Coord): Try[Boolean] = {
    if (koCoord != move.coord) { Success(true) } else Failure(new Exception("Cannot retake ko immediately"))
  }

  def check(move: Move)(position: Array[Int]): Try[Move] = {
    for {
      _ <- isEmptyPoint(move)(position)
      _ <- isNotSuicide(move)(position)
      _ <- isNotKo(move, board.ko)
    } yield move
  }

  //def check(move: Move): Try[Move] = { check(move, board.position) }

  def isEnemyStone(player: Int, color: Int): Boolean = {
    color != player && color != Board.EMPTY
  }

  def isKoMove(move: Move, captured: List[Int])(position: Array[Int]): Boolean = {
    captured.size == 1 && {
      neighbours(move.coord).forall( position(_) == Game.opponent(move.player) )
    }
  }

  def capturedStones(move: Move)(position: Array[Int]): List[Int] = {
    val enemies = neighbours(move.coord) filter ( x => isEnemyStone(move.player, position(x)) )
    // == 0 because move was executed by now
    ( enemies filter ( getLiberties(_)(position).size == 0 ) flatMap ( x => connComp(x)(position) ) ).distinct
  }

  def make(move: Move)(position: Array[Int]) {
    // Assume check for validity took place if needed.
    // Otherwise illegal positions may occur, which may be alright when setting up a position

    placeStone(move)(position)
    val captured = capturedStones(move)(position)
    nofCaptured(move.player) = nofCaptured(move.player) + captured.size

    // check for ko
    if (isKoMove(move, captured)(position)) {
      board.ko = Coord(captured.head)
    } else {
      board.ko = Coord.invalid
    }

    // remove captured stones
    placeStones(captured, Board.EMPTY)(position)

    // count moves up here seems to be an error. Only when use board.position
    //this.moveNr += 1
  }

  def makeMs(ms: List[Move]) {
    for(m <- ms) yield { make(m)(board.position) }
  }

  def legalMoves(player: Int)(position: Array[Int]): IndexedSeq[Move] = {
    for {
      p <- 0 until position.size
      move <- check(Move(Coord(p), player))(position).toOption
    } yield move
  }

  def randomMove(player: Int): Move = {
    val lm = this.legalMoves(player)(board.position)
    val r = Random
    val move = lm(r.nextInt(lm.size))
    move
  }

  def makeRandomMove(player: Int): Move = {
    val move = randomMove(player: Int)
    this.make(move)(board.position)
    this.player = Game.opponent(move.player)
    this.moveNr += 1
    move
  }

  def connComp(point: Int)(position: Array[Int]): List[Int] = {
    // get connected component of stones or territory
    val color = position(point)

    val stream = Stream[Int](point)
    val seen = Set[Int](point)
    val result = List()

    def helper(stream: Stream[Int], seen: Set[Int], result: List[Int]): List[Int] = {
      if (stream.isEmpty)
        result
      else {
        val p = stream.head
        val nbs = neighbours(p) filter (position(_) == color)
        helper(
          stream.tail ++ ( nbs filter ( !seen.contains(_) ) ),
          seen ++ nbs,
          p :: result
        )
      }
    }

    helper(stream, seen, result)
  }

  //def connComp(point: Int): List[Int] = {
  //  connComp(point)(board.position)
  //}

  def getLiberties(point: Int)(position: Array[Int]): List[Int] = {
    // have to check for BLACK and WHITE positively
    // check for not EMPTY fails by hover stones SEMI_BLACK, SEMI_WHITE
    // thats quite stupid; broken by design, etc..
    def isLiberty(p: Int): Boolean = {
      position(p) != Board.WHITE && position(p) != Board.BLACK
    }

    { connComp(point)(position) flatMap ( neighbours(_) filter isLiberty ) }.distinct
  }

  //def getLiberties(point: Int): List[Int] = {
  //  getLiberties(point)(board.position)
  //}

  def getLiberties(s: String)(position: Array[Int]): List[Int] = {
    // A1 -> `liberties`
    getLiberties(Coord(s))(board.position)
  }

  def connComps(color: Int = Board.EMPTY): List[IndexedSeq[Int]] = {
    // getConnectedComponents
    // inspired by example in https://en.wikipedia.org/wiki/Connected-component_labeling

    var nextLabel = 1
    val disjointSet = new DisjointSet[Int]

    val labels = new Array[Int](board.position.size)

    // first pass
    for(p <- 0 until board.position.size if board.position(p) == color) {
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

  def getRim(connectedComponent: Iterable[Int]): Array[Int] = {
    val ourFamily = connectedComponent.toSet
    val rs = for(c <- connectedComponent; n <- neighbours(c) if !ourFamily.contains(n)) yield n
    // remove duplicates
    rs.toArray.distinct // improve on this collections mess
  }

  def cleanColor(position: Array[Int]): Int = {
    if (position.forall(p => board.position(p) == Board.BLACK)) {
      Board.BLACK
    } else if (position.forall(p => board.position(p) == Board.WHITE)) {
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

  def score(): Double = {
    val territory = getCleanTerritory()

    // val blackTerritory = territory(Board.BLACK)
    // val whiteTerritory = territory(Board.WHITE)
    //val blackScore = blackTerritory + nofCaptured(Board.BLACK)
    //val whiteScore = whiteTerritory + nofCaptured(Board.WHITE) + this.komi

    val blackScore = nofCaptured(Board.BLACK)
    val whiteScore = nofCaptured(Board.WHITE) + this.komi

    blackScore - whiteScore
  }

  def getDisjointSet(color: Int)(position: Array[Int]): DisjointSet[Int] = {
    // inspired by example in https://en.wikipedia.org/wiki/Connected-component_labeling
    val disjointSet = new DisjointSet[Int]

    // first pass
    for(p <- 0 until position.size if position(p) == color) {
      disjointSet += p
      val ps = (p :: neighbours(p)) filter (x => position(x) == color && x <= p)
      for(l <- ps; k <- ps  if k > l) { disjointSet.union(l, k) }
    }
    disjointSet
  }

  def groupLiberties(cc: List[Int]): List[Int] = {
    val tmp = for(p <- cc; n <- neighbours(p) if board.position(n) == Board.EMPTY) yield n
    tmp.distinct
  }

  def getTerritories(point: Int): List[Iterable[Int]] = {
    val cc = connComp(point)(board.position)

    val dset = getDisjointSet(Board.EMPTY)(board.position)
    // F(ldf,f)i, pxF(lx

    val keys = for(liberty <- groupLiberties(cc)) yield { dset(liberty) }

    keys.distinct map (x => dset.set(x))
  }

  def getTerritories(s: String): List[Iterable[Int]] = {
    getTerritories(Coord(s))
  }
}

