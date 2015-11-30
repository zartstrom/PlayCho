import org.scalatest._

import shared.Board._
import shared.{Board,BoardSize,Game}


class SetsSpec extends FlatSpec with Matchers {
  "The function getDisjointSet" should "reflect the number of connected components on the board" in {
    implicit val boardSize = BoardSize(5, 5)
    val game = Game(boardSize)

    game.board.placeStones(List("C1", "C2", "C3", "C4", "C5"), BLACK)
    game.board.placeStones(List("B1", "B2", "B3", "B4", "B5"), WHITE)
    /*
         A B C D E
       5 ┌─○─●─┬─┐  5
       4 ├─○─●─┼─┤  4
       3 ├─○─●─┼─┤  3
       2 ├─○─●─┼─┤  2
       1 └─○─●─┴─┘  1
         A B C D E
    */
    game.connComps(EMPTY).size should be (2)
    game.connComps(BLACK).size should be (1)
    game.connComps(WHITE).size should be (1)

  }

  "The function getDisjointSet" should "reflect the number of connected components on the board II" in {
    /*
       A B C D E F G
     7 ┌─○─●─●─●─┬─●  7
     6 ○─┼─○─┼─●─●─┤  6
     5 ○─○─○─○─●─●─●  5
     4 ○─●─●─●─○─┼─┤  4
     3 ○─●─┼─●─○─○─○  3
     2 ●─┼─●─○─○─┼─○  2
     1 ●─●─●─●─┴─○─┘  1
       A B C D E F G
    */
    implicit val boardSize = BoardSize(7, 7)
    val game = Game(boardSize)

    val blackStones =
      List("A2", "A1", "B1", "C1", "C2") ++
      List("B3", "B4", "C4", "D4", "D3") ++
      List("E7", "E6", "F6", "F5", "G5", "G7") ++
      List("D7", "C7", "E5", "D1")
    val whiteStones =
      List("A6", "A5", "B5", "C5", "C6", "B7") ++
      List("E2", "E3", "F3", "G3", "G2", "F1") ++
      List("D2", "A4", "A3", "D5", "E4")
    game.board.placeStones(blackStones, BLACK)
    game.board.placeStones(whiteStones, WHITE)

    game.connComps(EMPTY).size should be (11)
    game.connComps(BLACK).size should be (4)
    game.connComps(WHITE).size should be (4)
  }
}
