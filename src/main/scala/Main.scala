import Game.*
import Grid.*

@main
def randomGame(): Unit =
  val game = Game(Player.X)
  play(game).drawGame()

def play(game: Game): Game = game match
  case Game(_, _, moves) if moves.isEmpty => game
  case Game(grid, player, moves) =>
    game.drawGame()
    play(game.makeMove(moves.toSeq(0)))


