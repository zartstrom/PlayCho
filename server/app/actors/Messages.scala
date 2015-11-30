package actors


import shared.{BoardSize,Game,Move}


object Msg {

  case class NewGame(boardSize: BoardSize)

  case class NewMove(move: Move)

  case object StartThinking

  case class StartEngine(game: Game)

  case class Simple(s: String)

  case object NewSocket

  case class CurrentBestMove(coord: String)

  case class GiveBestMove(game: Game)

  case class PlayOut(game: Game, lastMove: Move)

  case class PlayedOutResult(move: Move, result: score.Result)

}
