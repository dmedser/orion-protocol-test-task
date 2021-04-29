package app

import _root_.util.arbitrary.ArbitraryInstances
import _root_.util.spec.IOSpec
import app.model.Asset._
import app.model.Client
import fs2.Stream

final class OrderBooksSpec extends IOSpec with ArbitraryInstances {

  "OrderBooks" - {

    "stream" - {

      "balances invariance" in {

        runF {

          val clientSeq = generateClients

          val clientStream = Stream.emits(clientSeq).covary[F]

          val numberOfOrders = generateWithinRange(10000, 20000)

          val orderStream = Stream.emits(generateOrders(numberOfOrders)).covary[F]

          def sum(clients: Seq[Client], f: Client => Int): Int =
            clients.foldLeft(0) { case (acc, client) => acc + f(client) }

          val initTotalBalance = sum(clientSeq, _.balance)

          val initTotalAs = sum(clientSeq, _.assets(A))
          val initTotalBs = sum(clientSeq, _.assets(B))
          val initTotalCs = sum(clientSeq, _.assets(C))
          val initTotalDs = sum(clientSeq, _.assets(D))

          for {
            _                 <- console.print("Number of orders: ") >> console.println(numberOfOrders)
            clients           <- Clients.makeEffect[F](clientStream)
            _                 <- console.println("Clients before test: ") >> clients.print
            orderBooks        <- OrderBooks.makeEffect[F](orderStream, clients)
            _                 <- orderBooks.stream.compile.drain
            updatedClientsMap <- clients.get
            _                 <- console.println("Clients after test:") >> clients.print
          } yield {

            val updatedClients = updatedClientsMap.values.toSeq

            val finalTotalBalance = sum(updatedClients, _.balance)

            val finalTotalAs = sum(updatedClients, _.assets(A))
            val finalTotalBs = sum(updatedClients, _.assets(B))
            val finalTotalCs = sum(updatedClients, _.assets(C))
            val finalTotalDs = sum(updatedClients, _.assets(D))

            finalTotalBalance mustBe initTotalBalance

            finalTotalAs mustBe initTotalAs
            finalTotalBs mustBe initTotalBs
            finalTotalCs mustBe initTotalCs
            finalTotalDs mustBe initTotalDs
          }
        }
      }
    }
  }
}
