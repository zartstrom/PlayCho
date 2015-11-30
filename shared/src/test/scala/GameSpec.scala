import scala.util.{Try, Success, Failure}

import org.scalatest._
import scala.language.implicitConversions

import shared.{Board,BoardSize,Coord,Game,Move}
import shared.Board._


class GameSpec extends FlatSpec with Matchers {

  "An empty Board" should "provide as many legal moves as points on the board" in {
    val game = Game(19)
    game.legalMoves(BLACK).size should be (19 * 19)
  }
}


class LibertiesSpec extends FlatSpec with Matchers {
  "A stone" should "have two liberties in the corner" in {
    implicit val boardSize = BoardSize(19, 19)
    val game = Game(boardSize)

    val moves = List(
      Move(Coord("A1"), BLACK),
      Move(Coord("A19"), BLACK),
      Move(Coord("T1"), BLACK),
      Move(Coord("T19"), BLACK)
    )
    game.makeMoves(moves)
    game.liberties("A1").size  should be (2)
    game.liberties("A19").size should be (2)
    game.liberties("T1").size  should be (2)
    game.liberties("T19").size should be (2)
  }

  it should "have three liberties on the edge" in {
    implicit val boardSize = BoardSize(19, 19)
    val game = Game(boardSize)

    val moves = List(
      Move(Coord("A5"), BLACK),
      Move(Coord("F1"), BLACK),
      Move(Coord("T8"), BLACK),
      Move(Coord("M19"), BLACK)
    )
    game.makeMoves(moves)
    game.liberties("A5").size  should be (3)
    game.liberties("F1").size  should be (3)
    game.liberties("T8").size  should be (3)
    game.liberties("M19").size should be (3)
  }

  it should "have four liberties in the middle of the board" in {
    implicit val boardSize = BoardSize(19, 19)
    val game = Game(boardSize)

    val moves = List(
      Move(Coord("B5"), BLACK),
      Move(Coord("F2"), BLACK),
      Move(Coord("S8"), BLACK),
      Move(Coord("J10"), BLACK)
    )
    game.makeMoves(moves)
    game.liberties("B5").size  should be (4)
    game.liberties("F2").size  should be (4)
    game.liberties("S8").size  should be (4)
    game.liberties("J10").size should be (4)
  }

  "A group of stones" should "share the number of liberties" in {
    implicit val boardSize = BoardSize(19, 19)
    val game = Game(boardSize)

    val points = List("F3", "F4", "G4")
    game.makeMoves( points map (p => Move(Coord(p), BLACK) ) )

    (points map ( game.liberties(_).size )).toSet.size should be (1)
  }
}

class CaptureSpec extends FlatSpec with Matchers {
  "A move" should "capture an enemy stone if it takes the stone's last liberty" in {
    implicit val boardSize = BoardSize(19, 19)
    val game = Game(boardSize)
    game.board.placeStones(List("E3", "F4", "E5"), BLACK)
    game.board.placeStones(List("E4"), WHITE)

    game.isCapture( Move(Coord("D4"), BLACK) ) should be (true)
  }

  it should "capture an enemy group if it takes the group's last liberty" in {
    implicit val boardSize = BoardSize(19, 19)
    val game = Game(boardSize)
    game.board.placeStones(List("L1", "M2", "M3", "L4", "K3"), BLACK)
    game.board.placeStones(List("L2", "L3"), WHITE)

    game.isCapture( Move(Coord("K2"), BLACK) ) should be (true)
  }
}

class SuicideSpec extends FlatSpec with Matchers {
  "A suicidal move" should "be detected if a single stone has no liberties" in {
    implicit val boardSize = BoardSize(19, 19)
    val game = Game(boardSize)
    val moves = List(
      Move(Coord("F4"), BLACK),
      Move(Coord("E3"), BLACK),
      Move(Coord("E5"), BLACK),
      Move(Coord("D4"), BLACK)
    )
    game.makeMoves(moves)

    game.isSuicide( Move(Coord("E4"), BLACK) ) should be (false)
    game.isSuicide( Move(Coord("E4"), WHITE) ) should be (true)
  }

  it should "be detected if a group of stones has no liberties" in {
    implicit val boardSize = BoardSize(19, 19)
    val game = Game(boardSize)

    val points = List("L1", "M2", "M3", "L4", "K3", "K2")
    game.makeMoves( points map (p => Move(Coord(p), BLACK) ) )
    game.make( Move(Coord("L2"), WHITE) )

    game.isSuicide( Move(Coord("L3"), BLACK) ) should be (false)
    game.isSuicide( Move(Coord("L3"), WHITE) ) should be (true)
  }
}

class RemoveStonesSpec extends FlatSpec with Matchers {
  "A capture move" should "remove enemy stones from the board" in {
    implicit val boardSize = BoardSize(19, 19)
    val game = Game(boardSize)

    val blackStones = List("L1", "M2", "M3", "K3", "K2")
    val whiteStones = List("L2", "L3")
    game.board.placeStones(blackStones, BLACK)
    game.board.placeStones(whiteStones, WHITE)

    game.board.getStone("L2") should be (Board.WHITE)
    game.board.getStone("L3") should be (Board.WHITE)

    game.isCapture( Move(Coord("L4"), BLACK) ) should be (true)
    game.make( Move(Coord("L4"), BLACK) )

    game.board.getStone("L2") should be (Board.EMPTY)
    game.board.getStone("L3") should be (Board.EMPTY)
  }

  "A capture move" should "keep count of the number of captured stones" in {
    implicit val boardSize = BoardSize(19, 19)
    val game = Game(boardSize)
    val blackStones = List("L1", "M2", "M3", "M4", "K4", "K3", "K2")
    val whiteStones = List("L2", "L3", "L4")
    game.board.placeStones(blackStones, BLACK)
    game.board.placeStones(whiteStones, WHITE)

    game.make( Move(Coord("L5"), BLACK) )

    game.nofCaptured(BLACK) should be (3)
  }
}

class KoSpec extends FlatSpec with Matchers {
  "The ko rule" should "be in effect" in {
    implicit val boardSize = BoardSize(19, 19)
    val game = Game(boardSize)
    val blackStones = List("K2", "L1", "M2")
    val whiteStones = List("K3", "L4", "M3")
    game.board.placeStones(blackStones, BLACK)
    game.board.placeStones(whiteStones, WHITE)

    game.make( Move(Coord("L3"), BLACK) )

    game.check( Move(Coord("L2"), WHITE) ) should be a ('success)
    game.make( Move(Coord("L2"), WHITE) )

    game.check( Move(Coord("L3"), BLACK) ) should be a ('failure)
    game.make( Move(Coord("A1"), BLACK) )
    game.make( Move(Coord("B1"), WHITE) )

    game.check( Move(Coord("L3"), BLACK) ) should be a ('success)
  }

  it should "allow circles" in {
    implicit val boardSize = BoardSize(9, 9)
    val game = Game(boardSize)

    val blackStones = List("A3", "B4", "C3", "A8", "B7", "B9", "C8", "D3", "E4", "F3", "D8", "E7", "E9", "F8")
    val whiteStones = List("A2", "B1", "B3", "C2", "A7", "B6", "C7", "D2", "E1", "E3", "F2", "D7", "E6", "F7")

    game.board.placeStones(blackStones, BLACK)
    game.board.placeStones(whiteStones, WHITE)
    /*
       A B C D E F G H J
     9 ┌─●─┬─┬─●─┬─┬─┬─┐  9
     8 ●─┼─●─●─┼─●─┼─┼─┤  8
     7 ○─●─○─○─●─○─┼─┼─┤  7
     6 ├─○─┼─┼─○─┼─┼─┼─┤  6
     5 ├─┼─┼─┼─┼─┼─┼─┼─┤  5
     4 ├─●─┼─┼─●─┼─┼─┼─┤  4
     3 ●─○─●─●─○─●─┼─┼─┤  3
     2 ○─┼─○─○─┼─○─┼─┼─┤  2
     1 └─○─┴─┴─○─┴─┴─┴─┘  1
       A B C D E F G H J
    */ 

    game.check( Move(Coord("B8")(boardSize), WHITE) ) should be a ('success)  // [Move](_))
    game.make( Move(Coord("B8"), WHITE) )

    game.check( Move(Coord("B7"), BLACK) ) should be a ('failure)
    game.check( Move(Coord("B2"), BLACK) ) should be a ('success)
    game.make( Move(Coord("B2"), BLACK) )

    game.check( Move(Coord("B2"), WHITE) ) should be a ('failure)
    game.check( Move(Coord("E8"), WHITE) ) should be a ('success)
    game.make( Move(Coord("E8"), WHITE) )

    game.check( Move(Coord("E2"), BLACK) ) should be a ('success)
    game.make( Move(Coord("E2"), BLACK) )

    // restore start position
    game.check( Move(Coord("B3"), WHITE) ) should be a ('success)
    game.make( Move(Coord("B3"), WHITE) )

    game.check( Move(Coord("B7"), BLACK) ) should be a ('success)
    game.make( Move(Coord("B7"), WHITE) )

    game.check( Move(Coord("E3"), WHITE) ) should be a ('success)
    game.make( Move(Coord("E3"), WHITE) )

    game.check( Move(Coord("E7"), BLACK) ) should be a ('success)
    game.make( Move(Coord("E7"), WHITE) )
  }
}
