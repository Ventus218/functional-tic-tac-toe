import scala.io.StdIn.readLine
import Game.*

object GameIO:
  opaque type GameIO[T] = GameIOImpl[T]
  case class GameIOImpl[T](content: T)

  def apply(game: Game): GameIO[Game] = GameIOImpl(game)

  def printMessage(message: String): GameIO[Unit] =
    println(message)
    GameIOImpl({})

  def printGame(game: Game): GameIO[Unit] =
    import HorizontalPosition.{Center as HCenter, *}
    import VerticalPosition.{Center as VCenter, *}
    for x <- Seq(Left, HCenter, Right)
    do
      for y <- Seq(Top, VCenter, Bottom)
      do print(game.grid.cells((x, y)).map(_.toString()).getOrElse("_"))
      println()
    GameIOImpl({})

  def inputMove(game: Game): GameIO[Position] =
    val availableMoves = game.availableMoves.toSeq
    availableMoves.zipWithIndex
      .foreach((pos, i) => println(s"${i + 1} -> x: ${pos._1} y:${pos._2}"))
    val moveIndex = readLine("Choose your move: ")
    val i = moveIndex.toInt - 1 // TODO: how to handle errors here?
    GameIOImpl(availableMoves(i))

  extension [A](gameIO: GameIO[A])
    def map[B](f: A => B): GameIO[B] = GameIOImpl(f(gameIO.content))
    def flatMap[B](f: A => GameIO[B]): GameIO[B] = f(gameIO.content)
