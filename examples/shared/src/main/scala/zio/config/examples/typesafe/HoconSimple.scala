package zio.config.examples.typesafe

import zio.config.magnolia._
import zio.config.typesafe.TypesafeConfigSource
import zio._
import zio.Console._
import zio.config.examples.ZioOps

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
            street: Beresford
          }
        ]  
     }
    """

  val invalidCustomer =
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
      customer        <- TypesafeConfigSource.fromHoconString(customer).load(deriveConfig[Customer])
      invalidCustomer <- TypesafeConfigSource.fromHoconString(invalidCustomer).load(deriveConfig[Customer]).either
      _               <- printLine(customer)
      _               <- printLine(invalidCustomer)
    } yield ()

}
