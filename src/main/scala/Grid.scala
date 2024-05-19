object Grid:
  opaque type Grid = Set[(HorizontalPosition, VerticalPosition)]

  def apply(): Grid =
    (for
      x <- HorizontalPosition.values
      y <- VerticalPosition.values
    yield (x, y)).toSet

enum HorizontalPosition:
  case Left, Center, Right

enum VerticalPosition:
  case Top, Center, Bottom
