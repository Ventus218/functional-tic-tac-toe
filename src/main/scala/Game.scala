import Grid.*
import HorizontalPosition.{Center as HCenter, *}
import VerticalPosition.{Center as VCenter, *}

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
    def makeMove(move: Position): (Game, Option[Player]) = game match
      case Game(grid, Some(nextPlayer), availableMoves)
          if !availableMoves.isEmpty =>
        val newGrid = grid.place(move, nextPlayer)
        val winner = newGrid.winner
        val gameIsFinished = availableMoves.size == 1 || winner.isDefined
        (
          GameImpl(
            newGrid,
            if (gameIsFinished) then None else Some(nextPlayer.other),
            availableMoves - move
          ),
          winner
        )
      case _ => throw IllegalArgumentException()

    def isFinished: Boolean = game match
      case GameImpl(_, None, _)                                     => true
      case GameImpl(_, _, availableMoves) if availableMoves.isEmpty => true
      case _                                                        => false

    def tableAsString: String =
      val positions = for
        y <- Seq(Top, VCenter, Bottom)
        x <- Seq(Left, HCenter, Right)
      yield (x, y)

      positions
        .map(pos => game.grid.cells(pos).map(_.toString()).getOrElse("_"))
        .grouped(3)
        .map(_.mkString)
        .mkString("\n")

  extension (grid: Grid)
    private def winner: Option[Player] =
      Player.values.find(player =>
        val marks = grid.cells.collect { case (pos, Some(`player`)) =>
          pos
        }.toSeq
        marks.filter(_._1 == Left).size == 3 ||
        marks.filter(_._1 == HCenter).size == 3 ||
        marks.filter(_._1 == Right).size == 3 ||
        marks.filter(_._2 == Top).size == 3 ||
        marks.filter(_._2 == VCenter).size == 3 ||
        marks.filter(_._2 == Bottom).size == 3 ||
        marks
          .filter(pos =>
            pos == (Left, Top) || pos == (HCenter, VCenter) || pos == (Right, Bottom)
          )
          .size == 3 ||
        marks
          .filter(pos =>
            pos == (Right, Top) || pos == (HCenter, VCenter) || pos == (Left, Bottom)
          )
          .size == 3
      )
