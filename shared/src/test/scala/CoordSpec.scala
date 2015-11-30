import org.scalatest._

import shared.{BoardSize,Coord}


class CoordSpec extends FlatSpec with Matchers {

  "A coordinate" should "have a string representation" in {

    val coord = Coord(0)(BoardSize(19, 19))

    coord.toString should be ("A19")
  }
}
