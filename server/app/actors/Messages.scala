package messages


import shared.{BoardSize,Game,Move}


case class NewGame(boardSize: BoardSize)

case class NewMove(move: Move)

case object StartThinking

case class StartEngine(game: Game)

case class Simple(s: String)
