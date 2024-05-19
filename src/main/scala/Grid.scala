object Grid:
  opaque type Grid = Map[(HorizontalPosition, VerticalPosition), Option[Player]]

  def apply(): Grid =
    val positions = for
      x <- HorizontalPosition.values
      y <- VerticalPosition.values
    yield (x, y)
    Map.from(positions.map(pos => (pos, Option.empty)))

  extension (grid: Grid)
    def place(
        pos: (HorizontalPosition, VerticalPosition),
        player: Player
    ): Grid =
      grid.+((pos, Option(player)))

enum HorizontalPosition:
  case Left, Center, Right

enum VerticalPosition:
  case Top, Center, Bottom
