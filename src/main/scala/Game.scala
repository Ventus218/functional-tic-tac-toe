import Grid.*

object Game:
  opaque type Game = GameImpl
  case class GameImpl(
      grid: Grid,
      nextPlayer: Option[Player],
      availableMoves: Set[Position]
  )

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
    def availableMoves: Set[Position] = game.availableMoves

    def drawGame(): Unit =
      println("-------------")
      game.nextPlayer match
        case None         => println("Game finished!\n")
        case Some(player) => println(s"Turn: $player\n")

      import HorizontalPosition.{Center as HCenter, *}
      import VerticalPosition.{Center as VCenter, *}
      for x <- Seq(Left, HCenter, Right)
      do
        for y <- Seq(Top, VCenter, Bottom)
        do print(game.grid.cells((x, y)).map(_.toString()).getOrElse("_"))
        println()
