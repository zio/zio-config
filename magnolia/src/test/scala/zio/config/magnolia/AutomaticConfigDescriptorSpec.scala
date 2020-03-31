package zio.config.magnolia

import zio.config.{ read, write, BaseSpec, ConfigSource, PropertyTree }
import zio.config.magnolia.DeriveConfigDescriptor._
import AutomaticConfigTestUtils._
import zio.ZIO
import zio.random.Random
import zio.test._
import zio.config.helpers._
import zio.test.Assertion._

object AutomaticConfigTest
    extends BaseSpec(
      suite("magnolia spec")(
        testM("automatic derivation spec") {
          checkM(genEnvironment) {
            environment =>
              val configDesc = descriptor[MyConfig]

              val source =
                ConfigSource.fromMap(environment)

              val readAndWrite: ZIO[Any, Any, Either[String, PropertyTree[String, String]]] =
                for {
                  result  <- ZIO.fromEither(read(configDesc from source))
                  written <- ZIO.effectTotal(write(configDesc, result))
                } yield written

              val defaultValue   = environment.getOrElse("default", "1")
              val anotherDefault = environment.getOrElse("anotherDefault", "true")

              val updatedEnv =
                environment
                  .updated("default", defaultValue)
                  .updated("anotherDefault", anotherDefault)

              val actual = readAndWrite
                .map(_.map(_.flattenString()))
                .map(_.fold(_ => Nil, fromMultiMap(_).toList.sortBy(_._1)))

              assertM(actual)(equalTo(updatedEnv.toList.sortBy(_._1)))
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

  final case class Aws(region: String, security: Credentials)

  final case class DbUrl(dburl: String) extends AnyVal

  final case class MyConfig(
    aws: Aws,
    cost: Price,
    dburl: DbUrl,
    port: Int,
    amount: Option[Long],
    quantity: Either[Long, String],
    default: Int = 1,
    anotherDefault: Boolean = true
  )

  private val genPriceDescription                = genNonEmptyString(5).map(Description)
  private val genCurrency: Gen[Random, Currency] = Gen.double(10.0, 20.0).map(Currency)
  private val genPrice: Gen[Random, Price]       = Gen.oneOf(genPriceDescription, genCurrency)

  private val genToken       = genNonEmptyString(5).map(Token)
  private val genPassword    = genNonEmptyString(5).map(Password)
  private val genCredentials = Gen.oneOf(genToken, genPassword)

  private val genDbUrl = genNonEmptyString(5).map(DbUrl)

  private val genAws =
    for {
      region      <- genNonEmptyString(5)
      credentials <- genCredentials
    } yield Aws(region, credentials)

  private[magnolia] val genEnvironment =
    for {
      aws            <- genAws
      price          <- genPrice
      dbUrl          <- genDbUrl
      port           <- Gen.anyInt
      amount         <- Gen.option(Gen.long(1, 100))
      quantity       <- Gen.either(Gen.long(5, 10), genAlpha)
      default        <- Gen.option(Gen.anyInt)
      anotherDefault <- Gen.option(Gen.boolean)
      partialMyConfig = Map(
        "aws.region" -> aws.region,
        aws.security match {
          case Password(password) => "aws.security.credentials.password" -> password
          case Token(token)       => "aws.security.credentials.token"    -> token
        },
        price match {
          case Description(description) => "cost.price.description" -> description
          case Currency(dollars)        => "cost.price.dollars"     -> dollars.toString
        },
        "dburl.dburl" -> dbUrl.dburl,
        "port"        -> port.toString,
        "quantity"    -> quantity.fold(d => d.toString, s => s)
      ) ++ amount.map(double => ("amount", double.toString)).toList
    } yield (default, anotherDefault) match {
      case (Some(v1), Some(v2)) => partialMyConfig ++ List(("default", v1.toString), ("anotherDefault", v2.toString))
      case (Some(v1), None)     => partialMyConfig + (("default", v1.toString))
      case (None, Some(v2))     => partialMyConfig + (("anotherDefault", v2.toString))
      case (None, None)         => partialMyConfig
    }

  def genAlpha: Gen[Random, String] =
    for {
      n <- Gen.int(1, 10) // zio-config supports only cons hence starting with 1
      s <- Gen.listOfN(n)(Gen.char(65, 122))
    } yield s.mkString
}
