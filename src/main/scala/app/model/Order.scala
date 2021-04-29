package app.model

import app.util.parser._
import cats.MonadThrow
import cats.syntax.applicativeError._
import cats.syntax.flatMap._
import cats.syntax.functor._
import fs2.Pipe

final case class Order(clientId: Client.Id, action: Action, asset: Asset, price: Int, amount: Int)

object Order {

  private def parse[F[_] : MonadThrow](input: String): F[Order] =
    input.split("\\s").toList match {
      case id :: action :: asset :: price :: amount :: Nil =>
        for {
          id     <- Client.Id.parse(id)
          action <- Action.parse(action)
          asset  <- Asset.parse(asset)
          price  <- parseInt(price)
          amount <- parseInt(amount)
        } yield Order(id, action, asset, price, amount)
      case _ => new RuntimeException(s"Failed to parse order: $input").raiseError[F, Order]
    }

  def parser[F[_] : MonadThrow]: Pipe[F, String, Order] = _.evalMap(Order.parse[F])
}
