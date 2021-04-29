package app.model

import cats.ApplicativeThrow
import cats.syntax.option._

sealed trait Action extends Product with Serializable

object Action {

  case object Sell extends Action
  case object Buy  extends Action

  val all: Seq[Action] = Seq(Sell, Buy)

  def parse[F[_] : ApplicativeThrow](input: String): F[Action] = {
    input match {
      case "s" => Sell.some
      case "b" => Buy.some
      case _   => none[Action]
    }
  }.liftTo[F](new RuntimeException(s"Failed to parse action: $input"))
}
