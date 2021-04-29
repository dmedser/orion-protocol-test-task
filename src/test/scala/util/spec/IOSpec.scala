package util.spec

import cats.effect.IO
import cats.effect.std.Console
import cats.effect.unsafe.implicits.global

trait IOSpec extends BaseSpec {

  final type F[A] = IO[A]

  protected val console: Console[F] = implicitly[Console[F]]

  protected def runF[A](fa: F[A]): A = fa.unsafeRunSync()
}
