package zio.config.magnolia

import zio.ZIO
import zio.config.ConfigSource
import zio.config.magnolia.ConfigDescriptorProvider._
import AutomaticConfigTestUtils._
import zio.ZIO
import zio.random.Random
import zio.test._
import zio.config.helpers._
import zio.test.Assertion._
import zio.config.BaseSpec
import zio.config.PropertyTree
import zio.config.read
import zio.config.write
import zio.config.ReadErrors

object AutomaticConfigTest
    extends BaseSpec(
      suite("magnolia spec")(
        testM("automatic derivation spec") {
          checkM(genEnvironment) {
            p =>
              val configDesc = description[MyConfig]

              val source =
                ConfigSource.fromMap(p)

              val readAndWrite
                : ZIO[Any, ReadErrors[Vector[String], String], Either[String, PropertyTree[String, String]]] =
                for {
                  result  <- read(configDesc from source)
                  written <- ZIO.effectTotal(write(configDesc, result))
                } yield written

              val actual = readAndWrite
                .map(_.map(_.flattenString()))
                .map(_.fold(_ => Nil, _.toList.sortBy(_._1)))

              assertM(actual, equalTo(p.toList.sortBy(_._1)))
          }
        }
      )
    )

object AutomaticConfigTestUtils {
  sealed trait Credentials
  case class Password(password: String) extends Credentials
  case class Token(token: String)       extends Credentials

  sealed trait Price
  case class Description(description: String) extends Price
  case class Currency(dollars: Double)        extends Price

  final case class Aws(region: String, credentials: Credentials)

  final case class DbUrl(dburl: String) extends AnyVal

  final case class MyConfig(
    aws: Aws,
    price: Price,
    dburl: DbUrl,
    port: Int,
    amount: Option[Double],
    quantity: Either[Double, String],
    default: Int = 1,
    anotherDefault: Int = 2
  )

  val genPriceDescription                = genNonEmptyString(5).map(Description)
  val genCurrency: Gen[Random, Currency] = Gen.double(10.0, 20.0).map(Currency)
  val genPrice: Gen[Random, Price]       = Gen.oneOf(genPriceDescription, genCurrency)

  val genToken       = genNonEmptyString(5).map(Token)
  val genPassword    = genNonEmptyString(5).map(Password)
  val genCredentials = Gen.oneOf(genToken, genPassword)

  val genDbUrl = genNonEmptyString(5).map(DbUrl)

  val genAws =
    for {
      region      <- genNonEmptyString(5)
      credentials <- genCredentials
    } yield Aws(region, credentials)

  val genEnvironment =
    for {
      aws            <- genAws
      price          <- genPrice
      dbUrl          <- genDbUrl
      port           <- Gen.anyInt
      amount         <- Gen.option(Gen.double(1, 100))
      quantity       <- Gen.either(Gen.double(5, 10), genNonEmptyString(5))
      default        <- Gen.option(Gen.anyInt)
      anotherDefault <- Gen.option(Gen.anyInt)
      partialMyConfig = Map(
        "aws.region" -> aws.region,
        aws.credentials match {
          case Password(password) => "aws.credentials.password" -> password
          case Token(token)       => "aws.credentials.token"    -> token
        },
        price match {
          case Description(description) => "price.description" -> description
          case Currency(dollars)        => "price.dollars"     -> dollars.toString
        },
        "dburl.dburl" -> dbUrl.dburl,
        "port"        -> port.toString(),
        "quantity"    -> quantity.fold(d => d.toString, s => s)
      ) ++ amount.map(double => ("amount", double.toString())).toList
    } yield (default, anotherDefault) match {
      case (Some(v1), Some(v2)) => partialMyConfig ++ List(("default", v1.toString), ("anotherDefault", v2.toString))
      case (Some(v1), None)     => partialMyConfig + (("default", v1.toString))
      case (None, Some(v2))     => partialMyConfig + (("anotherDefault", v2.toString))
      case (None, None)         => partialMyConfig
    }
}
