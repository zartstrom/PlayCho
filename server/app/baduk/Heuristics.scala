package baduk

import shared.{Move}


object Heuristics {

  def noRim(moves: IndexedSeq[Move]): IndexedSeq[Move] = {
    moves filter (m => !m.coord.isRim)
  }
}
