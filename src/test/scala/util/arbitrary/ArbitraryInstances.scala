package util.arbitrary

import app.model.Asset._
import app.model.{Action, Asset, Client, Order}
import org.scalacheck.Gen.Choose
import org.scalacheck.{Arbitrary, Gen}

trait ArbitraryInstances {

  private val genClientId: Gen[Client.Id] = Gen.oneOf(Client.Id.all)

  private val genBalance: Gen[Int] = Gen.chooseNum(1000, 3000)

  private val genClientAssetAmount: Gen[Int] = Gen.chooseNum(500, 900)

  private val genOrderAssetAmount: Gen[Int] = Gen.chooseNum(1, 15)

  private val genAction: Gen[Action] = Gen.oneOf(Action.all)

  private val genAsset: Gen[Asset] = Gen.oneOf(Asset.all)

  private val genPrice: Gen[Int] = Gen.chooseNum(5, 20)

  protected implicit val arbClient: Arbitrary[Client] =
    Arbitrary {
      for {
        id      <- genClientId
        balance <- genBalance
        as      <- genClientAssetAmount
        bs      <- genClientAssetAmount
        cs      <- genClientAssetAmount
        ds      <- genClientAssetAmount
        assets = Map(A -> as, B -> bs, C -> cs, D -> ds)
      } yield Client(id, balance, assets)
    }

  protected implicit val arbOrder: Arbitrary[Order] =
    Arbitrary {
      for {
        id     <- genClientId
        action <- genAction
        asset  <- genAsset
        price  <- genPrice
        amount <- genOrderAssetAmount
      } yield Order(id, action, asset, price, amount)
    }

  def gen[T : Arbitrary]: T = Arbitrary.arbitrary[T].sample.get

  def generateWithinRange[T : Numeric : Choose](min: T, max: T): T =
    Gen.chooseNum(min, max).sample.get

  def generateClients: Seq[Client] =
    Client.Id.all.map(id => gen[Client].copy(id = id))

  def generateOrders(n: Int): List[Order] = List.fill(n)(gen[Order])
}
