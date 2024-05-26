import Game.*
import GameIO.*
import Grid.*

@main
def randomGame(): Unit =
  val game = Game(Player.X)
  play(GameIO(game))

def play(gameIO: GameIO[Game]): GameIO[Game] =
  for
    game <- gameIO
    _ <- printGame(game)
    _ <- print("Available moves:")
    move <- inputMove(game)
    newGame <- GameIO(game.makeMove(move)) // TODO: is this ok?
  yield newGame

