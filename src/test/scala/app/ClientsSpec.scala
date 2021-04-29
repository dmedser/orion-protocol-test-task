package app

import _root_.util.spec.IOSpec
import _root_.util.arbitrary.ArbitraryInstances
import app.model.Action._
import app.model._
import app.model.Client.Id._
import fs2.Stream

final class ClientsSpec extends IOSpec with ArbitraryInstances {

  "Clients" - {

    "changeBalances" in {

      val c1 = gen[Client].copy(id = C1)
      val c2 = gen[Client].copy(id = C2)

      val cs = Stream(c1, c2).covary[F]

      val o1 = gen[Order].copy(clientId = C1, action = Sell)
      val o2 = Order(clientId = C2, action = Buy, asset = o1.asset, price = o1.price, amount = o1.amount)

      runF {
        for {
          clients        <- Clients.makeEffect(cs)
          _              <- clients.changeBalances(o1, o2)
          updatedClients <- clients.get
        } yield {

          val updatedC1 = updatedClients(c1.id)
          val updatedC2 = updatedClients(c2.id)

          val totalPrice = o1.price * o1.amount

          updatedC1.balance mustBe c1.balance + totalPrice
          updatedC2.balance mustBe c2.balance - totalPrice

          updatedC1.assets(o1.asset) mustBe c1.assets(o1.asset) - o1.amount
          updatedC2.assets(o2.asset) mustBe c2.assets(o2.asset) + o2.amount
        }
      }
    }
  }
}
