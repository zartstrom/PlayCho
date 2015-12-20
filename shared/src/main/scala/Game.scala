package shared

import collection.mutable.ArrayBuffer
import collection.mutable.HashMap
import math.abs
import scala.util.Random
import scala.util.{Try, Success, Failure}


trait Play

//case class Pass() extends Play
case object Pass extends Play
case class Move(coord: Coord, player: Int) extends Play {
  // implementation for hashCode and equals follows example in http://www.artima.com/pins1ed/object-equality.html
  // additionally, suppose there is only one invalid coord
  override def hashCode = coord.hashCode * 41 + player
  override def equals(other: Any) = other match {
    case that: Move => this.coord == that.coord && this.player == that.player
    case _ => false
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

  // do it better, maybe convert Game to case class
  def clone(game: Game): Game = {
    // copy board as well
    val copy = new Game(Board.clone(game.board), game.komi, game.player)
    copy.moveNr = game.moveNr
    copy.nofCaptured = game.nofCaptured.clone()
    copy
  }
}

class Game(val board: Board, val komi: Double = 0.5, var player: Int = Board.BLACK) {
  // separated Board and Game classes; lets see how this plays out
  implicit val boardSize = board.boardSize

  var moveNr = 1
  var terminated = false
  var lastPass = -1
  var nofCaptured = HashMap(Board.BLACK -> 0, Board.WHITE -> 0)

  // keep it for later!
  //val displayer = new TerminalDisplay(this.size)
  //def display { this.displayer.display(board.position) }

  def placeStone(move: Move)(position: Array[Int]) {
    position(move.coord) = move.player
  }

  def getStone(point: Int): Int = { board.position(point) }
  def getStone(s: String): Int = { getStone(Coord(s)) }

  def placeStones(coords: List[Int], player: Int)(position: Array[Int]) {
    for(p <- coords) { placeStone(Move(Coord(p), player))(position) }
  }

  def isCapture(move: Move, position: Array[Int]): Boolean = {
    val enemies = board.neighbours(move.coord) filter ( x => { val p = position(x);  p != move.player && p != Board.EMPTY } )
    //println("enemies: %s".format(enemies))
    val enemyLiberties = enemies map ( liberties(_)(position).size )
    //println("liberties: %s".format(liberties))
    val killedNeighbors = enemyLiberties filter ( _ == 1 )

    killedNeighbors.size > 0
  }

  def isCapture(move: Move): Boolean = isCapture(move, board.position)

  def isSuicide(move: Move, position: Array[Int]): Boolean = {
    val isCapture: Boolean = this.isCapture(move, position)
    //println("isCapture: %s".format(isCapture.toString))

    if (isCapture) { false } else {
      val peek = position.clone
      placeStone(move)(peek)
      liberties(move.coord)(peek).size == 0
    }
  }

  def isSuicide(move: Move): Boolean = isSuicide(move, board.position)

  def isEmptyPoint(move: Move)(position: Array[Int]): Try[Boolean] = {
    if (position(move.coord) != Board.BLACK && position(move.coord) != Board.WHITE) {
      Success(true)
    } else Failure(new Exception("Place the stone on a empty point on the board"))
  }

  // make this snippet smarter
  def isNotSuicide(move: Move)(position: Array[Int]): Try[Boolean] = {
    val isCapture: Boolean = this.isCapture(move, position)
    //println("isCapture: %s".format(isCapture.toString))

    if (isCapture) { Success(true) } else {
      val peek = position.clone
      placeStone(move)(peek)
      if (liberties(move.coord)(peek).size == 0) {
        Failure(new Exception("Suicide is not allowed"))
      } else Success(true)
    }
  }

  def isNotKo(move: Move, koCoord: Coord): Try[Boolean] = {
    if (koCoord != move.coord) { Success(true) } else Failure(new Exception("Cannot retake ko immediately"))
  }

  def check(move: Move, position: Array[Int]): Try[Move] = {
    for {
      _ <- isEmptyPoint(move)(position)
      _ <- isNotSuicide(move)(position)
      _ <- isNotKo(move, board.ko)
    } yield move
  }

  def check(move: Move): Try[Move] = check(move, board.position)

  def isEnemyStone(player: Int, color: Int): Boolean = {
    color != player && color != Board.EMPTY
  }

  def isKoMove(move: Move, captured: List[Int])(position: Array[Int]): Boolean = {
    // check if move introduces a ko situation
    captured.size == 1 && {
      board.neighbours(move.coord).forall( position(_) == Game.opponent(move.player) )
    }
  }

  def capturedStones(move: Move)(position: Array[Int]): List[Int] = {
    val enemies = board.neighbours(move.coord) filter ( x => isEnemyStone(move.player, position(x)) )
    // == 0 because move was executed by now
    ( enemies filter ( liberties(_)(position).size == 0 ) flatMap ( x => connComp(x)(position) ) ).distinct
  }

  def make(play: Play, position: Array[Int]): Unit = {
    play match {
      case move: Move => makeMove(move, position)
      case Pass => makePass()
    }

    this.moveNr += 1
    if (this.moveNr > this.boardSize.nofPoints) {
      //Logger.error("Game has more than 400 moves, terminate condition is lousy")
      this.terminated = true
    }
    this.player = Game.opponent(player)
  }

  def make(play: Play): Unit = make(play, board.position)

  //def make(move: Move): Unit = make(move, board.position)

  def makePass(): Unit = {
    board.ko = Coord.invalid // not DRY
    this.terminated = (this.moveNr - 1) <= this.lastPass
    this.lastPass = this.moveNr
  }

  def makeMove(move: Move, position: Array[Int]): Unit = {
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
  }

  def makeMoves(ms: List[Move]) {
    for(m <- ms) yield { make(m) }
  }

  def legalMoves(player: Int, position: Array[Int]): IndexedSeq[Move] = {
    for {
      p <- 0 until position.size
      move <- check(Move(Coord(p), player), position).toOption
    } yield move
  }

  def legalMoves(player: Int): IndexedSeq[Move] = legalMoves(player, board.position)

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
        val nbs = board.neighbours(p) filter (position(_) == color)
        helper(
          stream.tail ++ ( nbs filter ( !seen.contains(_) ) ),
          seen ++ nbs,
          p :: result
        )
      }
    }

    helper(stream, seen, result)
  }

  def liberties(point: Int)(position: Array[Int]): List[Int] = {
    // have to check for BLACK and WHITE positively
    // check for not EMPTY fails by hover stones SEMI_BLACK, SEMI_WHITE
    // thats quite stupid; broken by design, etc..
    def isLiberty(p: Int): Boolean = {
      position(p) != Board.WHITE && position(p) != Board.BLACK
    }

    { connComp(point)(position) flatMap ( board.neighbours(_) filter isLiberty ) }.distinct
  }

  //def liberties(point: Int): List[Int] = {
  //  liberties(point)(board.position)
  //}

  def liberties(s: String): List[Int] = {
    // A1 -> `liberties`
    liberties(Coord(s))(board.position)
  }
}

