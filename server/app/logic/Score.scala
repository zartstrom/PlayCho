package score


import collection.mutable.HashMap

import shared.{Board,Coord,DisjointSet,Game}


object Result {
  def round(d: Double): Double = {
    (math rint d * 100000) / 100000
  }
}

case class Result(blackWins: Double, whiteWins: Double) {
  override def toString(): String = {
    this match {
      case Result(1, 0) => "Black wins"
      case Result(0, 1) => "White wins"
      case Result(0.5, 0.5) => "Tie game"
      case _ => "Black - White: %.1f - %.1f; avg: %f".format(blackWins, whiteWins, average)
    }
  }

  def record = blackWins - whiteWins

  def average: Double = {
    if (blackWins + whiteWins > 0) {
      Result.round(blackWins / (blackWins + whiteWins).toDouble)
    } else 0
  }

  def +(other: Result): Result = {
    Result(this.blackWins + other.blackWins, this.whiteWins + other.whiteWins)
  }

  def >(other: Result): Boolean = { this.average > other.average }
  def <(other: Result): Boolean = { this.average < other.average }
}


case object TrompTaylor {

  def score(game: Game): Double = {
    // count stones
    val nofBlackStones: Int = game.board.position count (_ == Board.BLACK) 
    val nofWhiteStones: Int = game.board.position count (_ == Board.WHITE) 

    def eval(point: Int): Int = {
      /**
       * return  1 if point has at least one black neighbor and no white neighbor
       * return -1 if point has at least one white neighbor and no black neighbor
       * return  0 otherwise
      **/
      val ns = game.board.neighbours(point) map (game.board.position(_))
      var result = 0
      if (ns contains Board.BLACK) { result +=  1 }
      if (ns contains Board.WHITE) { result += -1 }
      result
    }

    val territoryScore: Int = (game.board.position filter (_ == Board.EMPTY) map eval).sum

    nofBlackStones - nofWhiteStones + territoryScore
  }
}


class GameScore(game: Game) {
  val board = game.board
  implicit val boardSize = board.boardSize

  def connComps(color: Int = Board.EMPTY): List[IndexedSeq[Int]] = {
    // getConnectedComponents
    // inspired by example in https://en.wikipedia.org/wiki/Connected-component_labeling

    var nextLabel = 1
    val disjointSet = new DisjointSet[Int]

    val labels = new Array[Int](board.position.size)

    // first pass
    for(p <- 0 until board.position.size if board.position(p) == color) {
      val ns = board.neighbours(p) filter (x => labels(x) != 0)
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
    val rs = for(c <- connectedComponent; n <- board.neighbours(c) if !ourFamily.contains(n)) yield n
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

    val blackScore = game.nofCaptured(Board.BLACK)
    val whiteScore = game.nofCaptured(Board.WHITE) + game.komi

    blackScore - whiteScore
  }

  def getDisjointSet(color: Int)(position: Array[Int]): DisjointSet[Int] = {
    // inspired by example in https://en.wikipedia.org/wiki/Connected-component_labeling
    val disjointSet = new DisjointSet[Int]

    // first pass
    for(p <- 0 until position.size if position(p) == color) {
      disjointSet += p
      val ps = (p :: board.neighbours(p)) filter (x => position(x) == color && x <= p)
      for(l <- ps; k <- ps  if k > l) { disjointSet.union(l, k) }
    }
    disjointSet
  }

  def groupLiberties(cc: List[Int]): List[Int] = {
    val tmp = for(p <- cc; n <- board.neighbours(p) if board.position(n) == Board.EMPTY) yield n
    tmp.distinct
  }

  def getTerritories(point: Int): List[Iterable[Int]] = {
    val cc = game.connComp(point)(board.position)

    val dset = getDisjointSet(Board.EMPTY)(board.position)
    // F(ldf,f)i, pxF(lx

    val keys = for(liberty <- groupLiberties(cc)) yield { dset(liberty) }

    keys.distinct map (x => dset.set(x))
  }

  def getTerritories(s: String): List[Iterable[Int]] = {
    getTerritories(Coord(s))
  }
}
