package app.model

import app.model.Asset._
import app.util.parser._
import cats.syntax.applicativeError._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.option._
import cats.{ApplicativeThrow, Eq, MonadThrow}
import fs2.Pipe

final case class Client(id: Client.Id, balance: Int, assets: Map[Asset, Int]) {
  override def toString: String = s"$id $balance ${assets(A)} ${assets(B)} ${assets(C)} ${assets(D)}"
}

object Client {

  private final case class Dto(id: Client.Id, balance: Int, as: Int, bs: Int, cs: Int, ds: Int) {
    def toEntity: Client = Client(id, balance, Map(A -> as, B -> bs, C -> cs, D -> ds))
  }

  private object Dto {
    def parse[F[_] : MonadThrow](input: String): F[Dto] =
      input.split("\\s").toList match {
        case id :: balance :: as :: bs :: cs :: ds :: Nil =>
          for {
            id      <- Id.parse(id)
            balance <- parseInt(balance)
            as      <- parseInt(as)
            bs      <- parseInt(bs)
            cs      <- parseInt(cs)
            ds      <- parseInt(ds)
          } yield Dto(id, balance, as, bs, cs, ds)
        case _ => new RuntimeException(s"Failed to parse client: $input").raiseError[F, Dto]
      }
  }

  sealed trait Id

  object Id {

    case object C1 extends Id
    case object C2 extends Id
    case object C3 extends Id
    case object C4 extends Id
    case object C5 extends Id
    case object C6 extends Id
    case object C7 extends Id
    case object C8 extends Id
    case object C9 extends Id

    val all: Seq[Id] = Seq(C1, C2, C3, C4, C5, C6, C7, C8, C9)

    def parse[F[_] : ApplicativeThrow](input: String): F[Id] = {
      input match {
        case "C1" => C1.some
        case "C2" => C2.some
        case "C3" => C3.some
        case "C4" => C4.some
        case "C5" => C5.some
        case "C6" => C6.some
        case "C7" => C7.some
        case "C8" => C8.some
        case "C9" => C9.some
        case _    => none[Id]
      }
    }.liftTo[F](new RuntimeException(s"Failed to parse client id: $input"))

    implicit val eq: Eq[Id] = Eq.fromUniversalEquals[Id]

    implicit val ordering: Ordering[Id] =
      (x: Id, y: Id) => x.toString.compareTo(y.toString)
  }

  def parser[F[_] : MonadThrow]: Pipe[F, String, Client] = _.evalMap(Dto.parse[F]).map(_.toEntity)
}
