package app

import app.model.{Client, Order}
import cats.MonadThrow
import cats.effect.kernel.Concurrent
import cats.effect.std.Console
import cats.effect.{IO, IOApp}
import cats.syntax.flatMap._
import cats.syntax.functor._
import fs2.io.file.Files
import fs2.{Pipe, Stream, text}

import java.nio.file.Path

object Application extends IOApp.Simple {

  def stream[F[_] : Files : MonadThrow, A](path: Path)(parser: Pipe[F, String, A]): Stream[F, A] =
    Files[F]
      .readAll(path, 1024 * 1024)
      .through(text.utf8Decode)
      .through(text.lines)
      .through(parser)

  def orderStream[F[_] : Files : MonadThrow]: Stream[F, Order] =
    stream[F, Order](Path.of("./src/main/resources/orders.txt"))(Order.parser[F])

  def clientStream[F[_] : Files : MonadThrow]: Stream[F, Client] =
    stream[F, Client](Path.of("./src/main/resources/clients.txt"))(Client.parser[F])

  def program[F[_] : Files : Concurrent : Console]: F[Unit] =
    for {
      clients    <- Clients.makeEffect(clientStream)
      orderBooks <- OrderBooks.makeEffect(orderStream, clients)
      _          <- orderBooks.stream.compile.drain
      _          <- orderBooks.print
      _          <- clients.print
      _          <- clients.writeToFile(Path.of("./src/main/resources/result.txt"))
    } yield ()

  def run: IO[Unit] = program[IO]
}
