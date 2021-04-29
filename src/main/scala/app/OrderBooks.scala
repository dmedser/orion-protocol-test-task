package app

import app.model.{Asset, Order}
import cats.effect.kernel.Concurrent
import cats.effect.std.Console
import cats.syntax.eq._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import fs2.Stream

trait OrderBooks[F[_]] {
  def stream: Stream[F, Unit]
  def print: F[Unit]
}

object OrderBooks {

  def makeEffect[F[_] : Concurrent : Console](orders: Stream[F, Order], clients: Clients[F]): F[OrderBooks[F]] =
    for {
      orderBooks <- Asset.all.traverse(asset => OrderBook.makeEffect(clients).tupleLeft(asset))
    } yield new Impl(orders, orderBooks)

  private final class Impl[F[_] : Concurrent](orders: Stream[F, Order], orderBooks: Seq[(Asset, OrderBook[F])])(implicit
    console: Console[F]
  ) extends OrderBooks[F] {

    def stream: Stream[F, Unit] = {

      val workers = orderBooks.map { case (asset, orderBook) =>
        new Worker[F](orders.filter(_.asset === asset), orderBook)
      }

      Stream(workers.map(_.stream): _*).parJoinUnbounded
    }

    def print: F[Unit] =
      orderBooks.traverse { case (asset, orderBook) => console.println(s"Asset: $asset") >> orderBook.print }.void
  }

  private final class Worker[F[_]](orders: Stream[F, Order], orderBook: OrderBook[F]) {
    def stream: Stream[F, Unit] = orders.evalMap(orderBook.insert)
  }
}
