import scala.io.StdIn.readLine
import Game.*

object IO:
  opaque type IO[T] = IOImpl[T]
  case class IOImpl[T](content: T)

  def out(message: String): IO[Unit] = IO(println(message))

  def in(message: String = ""): IO[String] = IO(readLine(message))

  def inputMove(game: Game): IO[Position] =
    val availableMoves = game.availableMoves.toSeq
    availableMoves.zipWithIndex
      .foreach((pos, i) => println(s"${i + 1} -> x: ${pos._1} y:${pos._2}"))
    val moveIndex = readLine("Choose your move: ")
    val i = moveIndex.toInt - 1 // TODO: how to handle errors here?
    IO(availableMoves(i))

  def apply[T](content: T): IO[T] = IOImpl(content)

  extension [A](gameIO: IO[A])
    def map[B](f: A => B): IO[B] = IO(f(gameIO.content))
    def flatMap[B](f: A => IO[B]): IO[B] = f(gameIO.content)
