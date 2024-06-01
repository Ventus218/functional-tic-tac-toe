import Grid.*

trait GameTrait {
  val grid: Grid
  val nextPlayer: Option[Player]
  val availableMoves: Set[Position]
}

object Game:
  opaque type Game <: GameTrait = GameImpl
  case class GameImpl(
      grid: Grid,
      nextPlayer: Option[Player],
      availableMoves: Set[Position]
  ) extends GameTrait

  def apply(firstPlayer: Player): Game =
    val grid = Grid()
    GameImpl(grid, Option(firstPlayer), grid.emptyPositions)

  def unapply(game: Game): Option[(Grid, Option[Player], Set[Position])] =
    Some(game.grid, game.nextPlayer, game.availableMoves)

  extension (game: Game)
    def makeMove(move: Position): Game =
      require(game.nextPlayer.isDefined)
      require(game.availableMoves.contains(move))
      GameImpl(
        game.grid.place(move, game.nextPlayer.get),
        if game.availableMoves.size == 1 then None
        else game.nextPlayer.map(_.other),
        game.availableMoves - move
      )

    def tableAsString: String =
      import HorizontalPosition.{Center as HCenter, *}
      import VerticalPosition.{Center as VCenter, *}

      val positions = for
        y <- Seq(Top, VCenter, Bottom)
        x <- Seq(Left, HCenter, Right)
      yield (x, y)

      positions
        .grouped(3)
        .map(row =>
          row
            .map(pos => game.grid.cells(pos).map(_.toString()).getOrElse("_"))
            .mkString
        )
        .mkString("\n")
