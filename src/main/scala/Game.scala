import Grid.*

object Game:
  case class Game(
      grid: Grid,
      nextPlayer: Option[Player],
      availableMoves: Set[Position]
  )

  def apply(firstPlayer: Player): Game =
    val grid = Grid()
    Game(grid, Option(firstPlayer), grid.emptyPositions)

  extension (game: Game)
    def makeMove(move: Position): Game =
      require(game.nextPlayer.isDefined)
      require(game.availableMoves.contains(move))
      Game(
        game.grid.place(move, game.nextPlayer.get),
        game.nextPlayer.map(_.other),
        game.availableMoves - move
      )
