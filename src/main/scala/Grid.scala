object Grid:
  opaque type Grid = Map[Position, Option[Player]]

  def apply(): Grid =
    val positions = for
      x <- HorizontalPosition.values
      y <- VerticalPosition.values
    yield (x, y)
    Map.from(positions.map(pos => (pos, Option.empty)))

  extension (grid: Grid)
    def place(pos: Position, player: Player): Grid =
      grid.+((pos, Option(player)))

    def emptyPositions: Set[Position] =
      grid.filter(_._2.isEmpty).keySet

type Position = (HorizontalPosition, VerticalPosition)

enum HorizontalPosition:
  case Left, Center, Right

enum VerticalPosition:
  case Top, Center, Bottom
