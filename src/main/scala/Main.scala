import Game.*
import IO.*
import Grid.*

@main
def randomGame(): Unit =
  for
    (finishedGame, winner) <- play(Game(Player.X))
    _ <- out("\n\nGame finished!!")
    _ <- out(s"Winner: ${winner.map(_.toString()).getOrElse("None")}\n")
    _ <- out(finishedGame.tableAsString)
  yield {}

def play(game: Game): IO[(Game, Option[Winner])] =
  game.nextPlayer match
    case None => IO(game, game.winnerOption)
    case Some(player) =>
      for
        _ <- out("\n-------------\n")
        _ <- out(s"Turn: $player\n")
        _ <- out(game.tableAsString)
        move <- inputMove(game)
        (newGame, _) = game.makeMove(move)
        res <- play(newGame)
      yield res

def inputMove(game: Game): IO[Position] =
  for
    _ <- out("\nAvailable moves:")
    availableMoves = game.availableMoves.toSeq.sorted
    _ <- out(
      availableMoves.zipWithIndex
        .map((pos, i) => s"${i + 1} -> ${pos._1}-${pos._2}")
        .mkString("\n")
    )
    moveInput <- in("Choose your move: ")
    moveInputOption = moveInput.toIntOption
    validatedInput <- moveInputOption
      .filter(i => i > 0 && i <= availableMoves.size)
      .map(i => IO(availableMoves(i - 1)))
      .getOrElse:
        for
          _ <- out("Bad input, try again...")
          move <- inputMove(game)
        yield move
  yield validatedInput

given Ordering[Position] with
  override def compare(x: Position, y: Position): Int =
    Ordering[HorizontalPosition].compare(x._1, y._1) match
      case 0 => Ordering[VerticalPosition].compare(x._2, y._2)
      case r => r

import HorizontalPosition.{Center as HCenter, *}
import VerticalPosition.{Center as VCenter, *}

given Ordering[HorizontalPosition] with
  override def compare(x: HorizontalPosition, y: HorizontalPosition): Int =
    x match
      case x if x == y => 0
      case Left        => -1
      case HCenter     => if y == Left then 1 else -1
      case Right       => 1

given Ordering[VerticalPosition] with
  override def compare(x: VerticalPosition, y: VerticalPosition): Int =
    x match
      case x if x == y => 0
      case Top         => -1
      case VCenter     => if y == Top then 1 else -1
      case Bottom      => 1
