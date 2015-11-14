

import org.scalatest._

import baduk._
import baduk.Board._


class LibertiesSpec extends FlatSpec with Matchers {
  "A stone" should "have two liberties in the corner" in {
    val game = Game(19)

    game.board.placeStones( List("A1", "A19", "T1", "T19"), BLACK )
    game.getLiberties("A1").size  should be (2)
    game.getLiberties("A19").size should be (2)
    game.getLiberties("T1").size  should be (2)
    game.getLiberties("T19").size should be (2)
  }

  it should "have three liberties on the edge" in {
    val game = Game(19)

    game.board.placeStones( List("A5", "F1", "T8", "M19"), BLACK )
    game.getLiberties("A5").size  should be (3)
    game.getLiberties("F1").size  should be (3)
    game.getLiberties("T8").size  should be (3)
    game.getLiberties("M19").size should be (3)
  }

  it should "have four liberties in the middle of the board" in {
    val game = Game(19)

    game.board.placeStones( List("B5", "F2", "S8", "J10"), BLACK )
    game.getLiberties("B5").size  should be (4)
    game.getLiberties("F2").size  should be (4)
    game.getLiberties("S8").size  should be (4)
    game.getLiberties("J10").size should be (4)
  }

  "A group of stones" should "share the number of liberties" in {
    val game = Game(19)

    val ss = List("F3", "F4", "G4")

    game.board.placeStones(ss, BLACK)

    (ss map ( game.getLiberties(_) )).toSet.size should be (1)
    
  }
}

