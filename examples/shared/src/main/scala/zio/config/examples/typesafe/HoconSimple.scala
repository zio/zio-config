package zio.config.examples.typesafe

import zio.config.magnolia.DeriveConfig._
import zio.config.typesafe.TypesafeConfigSource
import zio._
import zio.Console._

object HoconSimple extends ZIOAppDefault {

  final case class Customer(name: String, orderIds: List[Int], address: List[Address])
  final case class Address(unit: Int, street: String, pin: Option[Int])

  val customer =
    s"""
     {
        name: Jon
        orderIds: [1,2,3]
        address : [
          {
            unit : 10
            street : Homebush,
            pin: 2135
          },
          {
            unit : 11
          }
        ]  
     }
    """

  def run =
    for {
      customer <- TypesafeConfigSource.fromHoconString_(customer).load(deriveConfig[Customer])
      _        <- printLine(customer)
    } yield ()

}
