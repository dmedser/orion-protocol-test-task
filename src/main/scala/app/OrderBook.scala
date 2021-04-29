package app

import app.OrderBook._
import app.model.Action._
import app.model._
import cats.Monad
import cats.effect.Ref
import cats.effect.kernel.Concurrent
import cats.effect.std.Console
import cats.syntax.eq._
import cats.syntax.flatMap._
import cats.syntax.functor._

import scala.collection.immutable.Queue

trait OrderBook[F[_]] {
  def insert(order: Order): F[Unit]
  def print: F[Unit]
  def get: F[States]
}

object OrderBook {

  type State = Map[Int, Map[Int, Queue[Order]]] // Map[Price, Map[Amount, Queue[Order]]]

  final case class States(sell: State, buy: State)

  def makeEffect[F[_] : Concurrent : Console](clients: Clients[F]): F[OrderBook[F]] =
    for {
      sell <- Ref.of(Map.empty[Int, Map[Int, Queue[Order]]])
      buy  <- Ref.of(Map.empty[Int, Map[Int, Queue[Order]]])
    } yield new Impl(sell, buy, clients)

  private final class Impl[F[_] : Monad](sell: Ref[F, State], buy: Ref[F, State], clients: Clients[F])(implicit
    console: Console[F]
  ) extends OrderBook[F] {

    def insert(order: Order): F[Unit] =
      order.action match {
        case Sell => handle(order, buy, sell)
        case Buy  => handle(order, sell, buy)
      }

    def print: F[Unit] =
      for {
        s <- sell.get
        b <- buy.get
        _ <- console.println(s"Sell: $s")
        _ <- console.println(s"Buy: $b")
      } yield ()

    def get: F[States] =
      for {
        s <- sell.get
        b <- buy.get
      } yield States(s, b)

    private def handle(order: Order, x: Ref[F, State], y: Ref[F, State]): F[Unit] =
      x.get.flatMap { prices =>
        prices.get(order.price).flatMap(_.get(order.amount)) match {
          case None => add(order, y)
          case Some(counterOrders) =>
            if (order.clientId === counterOrders.head.clientId)
              add(order, y)
            else
              delete(x, order.price, order.amount).flatMap(counterOrder => clients.changeBalances(order, counterOrder))
        }
      }

    private def add(order: Order, state: Ref[F, State]): F[Unit] =
      state.update { prices =>
        prices.get(order.price) match {
          case None => prices + (order.price -> Map(order.amount -> Queue(order)))
          case Some(amounts) =>
            val updatedAmounts =
              amounts.get(order.amount) match {
                case None         => amounts + (order.amount -> Queue(order))
                case Some(orders) => amounts.updated(order.amount, orders.enqueue(order))
              }
            prices.updated(order.price, updatedAmounts)
        }
      }

    private def delete(state: Ref[F, State], price: Int, amount: Int): F[Order] =
      state.modify { prices =>
        val amounts = prices(price)
        val (dequeuedOrder, updatedOrders) = amounts(amount).dequeue
        val updatedAmounts = {
          if (updatedOrders.isEmpty)
            amounts - amount
          else
            amounts.updated(amount, updatedOrders)
        }
        val updatedPrices = {
          if (updatedAmounts.isEmpty)
            prices - price
          else
            prices.updated(price, updatedAmounts)
        }
        updatedPrices -> dequeuedOrder
      }
  }
}
