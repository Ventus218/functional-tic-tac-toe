import scala.io.StdIn.readLine
import Game.*

object IO:
  opaque type IO[T] = IOImpl[T]
  case class IOImpl[T](content: T)

  def out(message: String): IO[Unit] = IO(println(message))

  def in(suggestion: String = ""): IO[String] = IO(readLine(suggestion))

  def apply[T](content: T): IO[T] = IOImpl(content)

  extension [A](io: IO[A])
    def map[B](f: A => B): IO[B] = IO(f(io.content))
    def flatMap[B](f: A => IO[B]): IO[B] = f(io.content)
