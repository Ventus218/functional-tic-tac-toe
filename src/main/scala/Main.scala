import Game.*
import GameIO.*
import Grid.*

@main
def randomGame(): Unit =
  for
    finishedGame <- play(Game(Player.X))
    _ <- printMessage("\n\nGame finished!!\n")
    _ <- printGame(finishedGame)
  yield {}

def play(g: Game): GameIO[Game] =
  g.nextPlayer match
    case None => GameIO(g)
    case Some(player) =>
      for
        game <- GameIO(g)
        _ <- printMessage("\n-------------\n")
        _ <- printMessage(s"Turn: $player\n")
        _ <- printGame(game)
        _ <- printMessage("\nAvailable moves:")
        move <- inputMove(game)
        newGame <- GameIO(game.makeMove(move)) // TODO: is this ok?
        res <-
          if (newGame.availableMoves.isEmpty) then GameIO(newGame)
          else play(newGame)
      yield res
