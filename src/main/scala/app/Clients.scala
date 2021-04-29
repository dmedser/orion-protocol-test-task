package app

import app.model.Action.{Buy, Sell}
import app.model.{Client, Order}
import cats.Monad
import cats.effect.Ref
import cats.effect.kernel.Concurrent
import cats.effect.std.Console
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import fs2.io.file.Files
import fs2.{INothing, Pipe, Stream, text}

import java.nio.file.Path
import scala.collection.immutable.TreeMap

trait Clients[F[_]] {
  def changeBalances(order: Order, counterOrder: Order): F[Unit]
  def print: F[Unit]
  def writeToFile(path: Path): F[Unit]
  def get: F[TreeMap[Client.Id, Client]]
}

object Clients {

  def makeEffect[F[_] : Concurrent : Files : StreamCompiler : Console](stream: Stream[F, Client]): F[Clients[F]] =
    for {
      list  <- stream.compile.toList
      state <- Ref.of[F, TreeMap[Client.Id, Client]](TreeMap.from(list.map(client => client.id -> client).toMap))
    } yield new Impl(state)

  private final class Impl[F[_] : Monad : Files : StreamCompiler](state: Ref[F, TreeMap[Client.Id, Client]])(implicit
    console: Console[F]
  ) extends Clients[F] {

    def changeBalances(order: Order, counterOrder: Order): F[Unit] =
      state.update { clients =>
        val (seller, buyer) = order.action match {
          case Sell => clients(order.clientId)        -> clients(counterOrder.clientId)
          case Buy  => clients(counterOrder.clientId) -> clients(order.clientId)
        }

        val totalPrice = order.price * order.amount

        val updatedSeller =
          seller.copy(
            balance = seller.balance + totalPrice,
            assets = seller.assets.updated(order.asset, seller.assets(order.asset) - order.amount)
          )

        val updatedBuyer =
          buyer.copy(
            balance = buyer.balance - totalPrice,
            assets = buyer.assets.updated(order.asset, buyer.assets(order.asset) + order.amount)
          )

        clients.updated(seller.id, updatedSeller).updated(buyer.id, updatedBuyer)
      }

    def print: F[Unit] =
      state.get.flatMap(_.toList.traverse(console.println).void)

    def writeToFile(path: Path): F[Unit] =
      state.get.flatMap { clients =>
        Stream
          .emits(clients.values.map(_.toString).toSeq)
          .intersperse("\n")
          .through(fileWriter(path))
          .compile
          .drain
      }

    def get: F[TreeMap[Client.Id, Client]] = state.get

    private def fileWriter(path: Path): Pipe[F, String, INothing] =
      _.through(text.utf8Encode).through(Files[F].writeAll(path))
  }
}
