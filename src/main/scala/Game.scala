import Grid.*

case class Game(
    grid: Grid,
    nextPlayer: Option[Player],
    availableMoves: Set[(HorizontalPosition, VerticalPosition)]
)

extension (game: Game)
  def makeMove(move: (HorizontalPosition, VerticalPosition)): Game =
    require(game.nextPlayer.isDefined)
    require(game.availableMoves.contains(move))
    Game(
      game.grid.place(move, game.nextPlayer.get),
      game.nextPlayer.map(_.other),
      game.availableMoves - move
    )
