package app

import _root_.util.arbitrary.ArbitraryInstances
import _root_.util.spec.IOSpec
import app.model.Action._
import app.model.Client.Id._
import app.model.Order
import fs2.Stream

import scala.collection.immutable.Queue

final class OrderBookSpec extends IOSpec with ArbitraryInstances {

  "OrderBook" - {

    def withOrderBook[A](test: OrderBook[F] => F[A]): F[A] = {

      val clientSeq = generateClients
      val clientStream = Stream.emits(clientSeq).covary[F]

      for {
        clients   <- Clients.makeEffect[F](clientStream)
        orderBook <- OrderBook.makeEffect[F](clients)
        a         <- test(orderBook)
      } yield a
    }

    "insert" - {

      "sell" - {

        "handle" - {

          "add" - {

            "price doesn't exist" in {

              val order = gen[Order].copy(action = Sell)

              runF {
                withOrderBook { orderBook =>
                  for {
                    _                <- orderBook.insert(order)
                    updatedOrderBook <- orderBook.get
                  } yield {
                    updatedOrderBook.buy mustBe Map.empty[Int, Map[Int, Queue[Order]]]
                    updatedOrderBook.sell mustBe Map(order.price -> Map(order.amount -> Queue(order)))
                  }
                }
              }
            }

            "price exists" - {

              "amount doesn't exist" in {

                val o1 = gen[Order].copy(action = Sell)
                val o2 = gen[Order].copy(action = Sell, price = o1.price, amount = o1.amount + 1)

                runF {
                  withOrderBook { orderBook =>
                    for {
                      _                <- orderBook.insert(o1)
                      _                <- orderBook.insert(o2)
                      updatedOrderBook <- orderBook.get
                    } yield {
                      updatedOrderBook.buy mustBe Map.empty[Int, Map[Int, Queue[Order]]]
                      updatedOrderBook.sell(o1.price) must contain theSameElementsAs
                        Map(o1.amount -> Queue(o1), o2.amount -> Queue(o2))
                    }
                  }
                }
              }

              "amount exists" in {

                val o1 = gen[Order].copy(action = Sell)
                val o2 = gen[Order].copy(action = Sell, price = o1.price, amount = o1.amount)

                runF {
                  withOrderBook { orderBook =>
                    for {
                      _                <- orderBook.insert(o1)
                      _                <- orderBook.insert(o2)
                      updatedOrderBook <- orderBook.get
                    } yield {
                      updatedOrderBook.buy mustBe Map.empty[Int, Map[Int, Queue[Order]]]
                      updatedOrderBook.sell(o1.price) must contain theSameElementsAs
                        Map(o1.amount -> Queue(o1, o2))
                    }
                  }
                }
              }
            }

            "counterorder client id is equal to order client id" in {

              val o1 = gen[Order].copy(clientId = C1, action = Buy)
              val o2 = o1.copy(clientId = C2)
              val o3 = o1.copy(action = Sell)

              runF {
                withOrderBook { orderBook =>
                  for {
                    _                <- orderBook.insert(o1)
                    _                <- orderBook.insert(o2)
                    _                <- orderBook.insert(o3)
                    updatedOrderBook <- orderBook.get
                  } yield {
                    updatedOrderBook.buy mustBe Map(o1.price -> Map(o1.amount -> Queue(o1, o2)))
                    updatedOrderBook.sell(o3.price) mustBe Map(o3.amount -> Queue(o3))
                  }
                }
              }
            }
          }

          "delete" - {

            "more than one order in queue" in {

              val s = gen[Order].copy(clientId = C1, action = Sell)
              val b1 = Order(clientId = C2, action = Buy, asset = s.asset, price = s.price, amount = s.amount)
              val b2 = b1.copy(clientId = C3)
              val b3 = b1.copy(clientId = C4)

              runF {
                withOrderBook { orderBook =>
                  for {
                    _                <- orderBook.insert(b1)
                    _                <- orderBook.insert(b2)
                    _                <- orderBook.insert(b3)
                    _                <- orderBook.insert(s)
                    updatedOrderBook <- orderBook.get
                  } yield {
                    updatedOrderBook.sell mustBe Map.empty[Int, Map[Int, Queue[Order]]]
                    updatedOrderBook.buy mustBe Map(b1.price -> Map(b1.amount -> Queue(b2, b3)))
                  }
                }
              }
            }

            "one order in queue" - {

              "more than one amount by price" in {

                val s = gen[Order].copy(clientId = C1, action = Sell)
                val b1 = Order(clientId = C2, action = Buy, asset = s.asset, price = s.price, amount = s.amount)
                val b2 = b1.copy(clientId = C3, amount = b1.amount + 1)
                val b3 = b2.copy(clientId = C4)

                runF {
                  withOrderBook { orderBook =>
                    for {
                      _                <- orderBook.insert(b1)
                      _                <- orderBook.insert(b2)
                      _                <- orderBook.insert(b3)
                      _                <- orderBook.insert(s)
                      updatedOrderBook <- orderBook.get
                    } yield {
                      updatedOrderBook.sell mustBe Map.empty[Int, Map[Int, Queue[Order]]]
                      updatedOrderBook.buy mustBe Map(b2.price -> Map(b2.amount -> Queue(b2, b3)))
                    }
                  }
                }
              }

              "one amount by price" in {

                val s = gen[Order].copy(clientId = C1, action = Sell)
                val b1 = Order(clientId = C2, action = Buy, asset = s.asset, price = s.price, amount = s.amount)
                val b2 = gen[Order].copy(clientId = C3, action = Buy, price = b1.price + 1)

                runF {
                  withOrderBook { orderBook =>
                    for {
                      _                <- orderBook.insert(b1)
                      _                <- orderBook.insert(b2)
                      _                <- orderBook.insert(s)
                      updatedOrderBook <- orderBook.get
                    } yield {
                      updatedOrderBook.sell mustBe Map.empty[Int, Map[Int, Queue[Order]]]
                      updatedOrderBook.buy mustBe Map(b2.price -> Map(b2.amount -> Queue(b2)))
                    }
                  }
                }
              }
            }
          }
        }
      }

      "buy" - {

        "handle" - {

          "add" - {

            "price doesn't exist" in {

              val order = gen[Order].copy(action = Buy)

              runF {
                withOrderBook { orderBook =>
                  for {
                    _                <- orderBook.insert(order)
                    updatedOrderBook <- orderBook.get
                  } yield {
                    updatedOrderBook.buy mustBe Map(order.price -> Map(order.amount -> Queue(order)))
                    updatedOrderBook.sell mustBe Map.empty[Int, Map[Int, Queue[Order]]]
                  }
                }
              }
            }

            "price exists" - {

              "amount doesn't exist" in {

                val o1 = gen[Order].copy(action = Buy)
                val o2 = gen[Order].copy(action = Buy, price = o1.price, amount = o1.amount + 1)

                runF {
                  withOrderBook { orderBook =>
                    for {
                      _                <- orderBook.insert(o1)
                      _                <- orderBook.insert(o2)
                      updatedOrderBook <- orderBook.get
                    } yield {
                      updatedOrderBook.buy(o1.price) must contain theSameElementsAs
                        Map(o1.amount -> Queue(o1), o2.amount -> Queue(o2))
                      updatedOrderBook.sell mustBe Map.empty[Int, Map[Int, Queue[Order]]]
                    }
                  }
                }
              }

              "amount exists" in {

                val o1 = gen[Order].copy(action = Buy)
                val o2 = gen[Order].copy(action = Buy, price = o1.price, amount = o1.amount)

                runF {
                  withOrderBook { orderBook =>
                    for {
                      _                <- orderBook.insert(o1)
                      _                <- orderBook.insert(o2)
                      updatedOrderBook <- orderBook.get
                    } yield {
                      updatedOrderBook.buy(o1.price) must contain theSameElementsAs
                        Map(o1.amount -> Queue(o1, o2))
                      updatedOrderBook.sell mustBe Map.empty[Int, Map[Int, Queue[Order]]]
                    }
                  }
                }
              }
            }

            "counterorder client id is equal to order client id" in {

              val o1 = gen[Order].copy(clientId = C1, action = Sell)
              val o2 = o1.copy(clientId = C2)
              val o3 = o1.copy(action = Buy)

              runF {
                withOrderBook { orderBook =>
                  for {
                    _                <- orderBook.insert(o1)
                    _                <- orderBook.insert(o2)
                    _                <- orderBook.insert(o3)
                    updatedOrderBook <- orderBook.get
                  } yield {
                    updatedOrderBook.buy(o3.price) mustBe Map(o3.amount -> Queue(o3))
                    updatedOrderBook.sell mustBe Map(o1.price -> Map(o1.amount -> Queue(o1, o2)))
                  }
                }
              }
            }
          }

          "delete" - {

            "more than one order in queue" in {

              val b = gen[Order].copy(clientId = C1, action = Buy)
              val s1 = Order(clientId = C2, action = Sell, asset = b.asset, price = b.price, amount = b.amount)
              val s2 = s1.copy(clientId = C3)
              val s3 = s1.copy(clientId = C4)

              runF {
                withOrderBook { orderBook =>
                  for {
                    _                <- orderBook.insert(s1)
                    _                <- orderBook.insert(s2)
                    _                <- orderBook.insert(s3)
                    _                <- orderBook.insert(b)
                    updatedOrderBook <- orderBook.get
                  } yield {
                    updatedOrderBook.sell mustBe Map(s1.price -> Map(s1.amount -> Queue(s2, s3)))
                    updatedOrderBook.buy mustBe Map.empty[Int, Map[Int, Queue[Order]]]
                  }
                }
              }
            }

            "one order in queue" - {

              "more than one amount by price" in {

                val b = gen[Order].copy(clientId = C1, action = Buy)
                val s1 = Order(clientId = C2, action = Sell, asset = b.asset, price = b.price, amount = b.amount)
                val s2 = s1.copy(clientId = C3, amount = s1.amount + 1)
                val s3 = s2.copy(clientId = C4)

                runF {
                  withOrderBook { orderBook =>
                    for {
                      _                <- orderBook.insert(s1)
                      _                <- orderBook.insert(s2)
                      _                <- orderBook.insert(s3)
                      _                <- orderBook.insert(b)
                      updatedOrderBook <- orderBook.get
                    } yield {
                      updatedOrderBook.sell mustBe Map(s2.price -> Map(s2.amount -> Queue(s2, s3)))
                      updatedOrderBook.buy mustBe Map.empty[Int, Map[Int, Queue[Order]]]
                    }
                  }
                }
              }

              "one amount by price" in {

                val b = gen[Order].copy(clientId = C1, action = Buy)
                val s1 = Order(clientId = C2, action = Sell, asset = b.asset, price = b.price, amount = b.amount)
                val s2 = gen[Order].copy(clientId = C3, action = Sell, price = s1.price + 1)

                runF {
                  withOrderBook { orderBook =>
                    for {
                      _                <- orderBook.insert(s1)
                      _                <- orderBook.insert(s2)
                      _                <- orderBook.insert(b)
                      updatedOrderBook <- orderBook.get
                    } yield {
                      updatedOrderBook.sell mustBe Map(s2.price -> Map(s2.amount -> Queue(s2)))
                      updatedOrderBook.buy mustBe Map.empty[Int, Map[Int, Queue[Order]]]
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}
