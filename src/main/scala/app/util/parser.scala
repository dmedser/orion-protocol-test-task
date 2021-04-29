package app.util

import cats.ApplicativeThrow
import cats.syntax.try_._

import scala.util.Try

object parser {
  def parseInt[F[_] : ApplicativeThrow](input: String): F[Int] = Try(input.toInt).liftTo[F]
}
