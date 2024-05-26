import Game.*
import IO.*
import Grid.*

@main
def randomGame(): Unit =
  for
    finishedGame <- play(Game(Player.X))
    _ <- out("\n\nGame finished!!\n")
    _ <- out(finishedGame.tableAsString)
  yield {}

def play(g: Game): IO[Game] =
  g.nextPlayer match
    case None => IO(g)
    case Some(player) =>
      for
        game <- IO(g)
        _ <- out("\n-------------\n")
        _ <- out(s"Turn: $player\n")
        _ <- out(game.tableAsString)
        _ <- out("\nAvailable moves:")
        move <- inputMove(game)
        newGame <- IO(game.makeMove(move)) // TODO: is this ok?
        res <-
          if (newGame.availableMoves.isEmpty) then IO(newGame)
          else play(newGame)
      yield res
