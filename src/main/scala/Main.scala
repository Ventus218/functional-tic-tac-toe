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
        availableMoves = game.availableMoves.toSeq // TODO: is this ok?
        _ <- out(
          availableMoves.zipWithIndex
            .map((pos, i) => s"${i + 1} -> ${pos._1}-${pos._2}")
            .mkString("\n")
        )
        moveInput <- in("Choose your move: ")
        moveIndex = moveInput.toInt - 1 // TODO: how to handle validation errors
        newGame <- IO(game.makeMove(availableMoves(moveIndex))) // TODO: is it ok to wrap into IO?
        res <-
          if (newGame.availableMoves.isEmpty) then IO(newGame)
          else play(newGame)
      yield res
