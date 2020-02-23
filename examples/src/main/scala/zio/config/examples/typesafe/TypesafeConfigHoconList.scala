package zio.config.examples.typesafe

import zio.DefaultRuntime
import zio.config.ConfigDescriptor.{ int, list, nested, string }
//import zio.config.ReadError.{ AndErrors, MissingValue, ParseError }
//import zio.config.magnolia.ConfigDescriptorProvider.description
import zio.config.read
import zio.config.typesafe.TypeSafeConfigSource.hocon
import zio.config._

object TypesafeConfigHoconList extends App {
  val runtime = new DefaultRuntime {}

  // A nested example with type safe config, and usage of magnolia
  final case class Details(name: Int, age: Int)

  final case class Account(accountId: String, regions: List[String], details: Option[Details])

  // final case class Database(port: Option[Int], url: String)

  // final case class AwsDetails(accounts: List[Account], database: Database, users: List[Int])

  val validHocon =
    """
    accounts = [
      {
          accountId: jon
          regions : [ss, aa, bb]
          details {
            name : 1
            age  : 10
          } 
      }
      {
          accountId: aa
                    regions : [ss, aa, bb]

          details {
            name : 1
            age: 101 
          } 
      }
      {
          accountId: bb
                    regions : [ss, aa, bb]

  

      }
    ]

    """

  val details = (int("name") |@| int("age"))(Details.apply, Details.unapply)

  val accountConfig =
    (string("accountId") |@| list(string("regions")) |@| nested("details")(
      details
    ).optional)(
      Account.apply,
      Account.unapply
    )

  //val databaseConfig = (int("port").optional |@| string("url"))(Database.apply, Database.unapply)
  /*
  val awsDetailsConfig =
    (nested("accounts")(list(accountConfig)) |@| nested("database")(databaseConfig) |@| list(int("users")))(
      AwsDetails.apply,
      AwsDetails.unapply
    )
   */
  val listResult =
    read(nested("accounts")(list(accountConfig)) from hocon(Right(validHocon)))

  println(runtime.unsafeRun(listResult))

  /*
  val automaticAwsDetailsConfig = description[AwsDetails]

  println(runtime.unsafeRun(read(automaticAwsDetailsConfig from hocon(Right(validHocon)))))

  // If defining such a configuration description is tedious, you may rely on (experimental) magnolia module

  val invalidHocon =
    """
    accounts = [
      {
          accountId: jon
      }
      {
          region : us-west
          accountId: chris
      }
      {
          accountId: null
      }
    ]

    users = [1, 2, 3]

    database {
        port : 1abcd
        url  : postgres
    }

    """

  println(runtime.unsafeRun(read(description[AwsDetails] from hocon(Right(invalidHocon))).either))
 */

}
