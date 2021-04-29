package app.model

import cats.syntax.option._
import cats.{ApplicativeThrow, Eq}

sealed trait Asset extends Product with Serializable

object Asset {

  case object A extends Asset
  case object B extends Asset
  case object C extends Asset
  case object D extends Asset

  val all: Seq[Asset] = Seq(A, B, C, D)

  def parse[F[_] : ApplicativeThrow](input: String): F[Asset] = {
    input match {
      case "A" => A.some
      case "B" => B.some
      case "C" => C.some
      case "D" => D.some
      case _   => none[Asset]
    }
  }.liftTo[F](new RuntimeException(s"Failed to parse asset: $input"))

  implicit val eq: Eq[Asset] = Eq.fromUniversalEquals[Asset]
}
