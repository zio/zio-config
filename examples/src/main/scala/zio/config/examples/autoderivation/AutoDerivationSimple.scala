package zio.config.examples.autoderivation

import zio.ConfigProvider
import zio.config._
import zio.config.examples.ZioOps
import zio.config.examples.autoderivation.MyConfig._

import magnolia._

final case class MyConfig(
  aws: Aws,
  price: Price,
  dburl: DbUrl,
  port: Int,
  amount: Double,
  quanity: Double,
  default: Int,
  anotherDefault: Int
)

object MyConfig {

  sealed trait Credentials
  case class Password(value: String) extends Credentials
  case class Token(value: String)    extends Credentials

  sealed trait Price
  case class Inr(value: Int)      extends Price
  case class Aud(dollars: Double) extends Price

  final case class Aws(region: String, credentials: Credentials)
  final case class DbUrl(value: String)
}

object AutomaticConfig extends App {
  private val automaticConfig = deriveConfig[MyConfig]

  private val source =
    ConfigProvider.fromMap(
      Map(
        "aws.region"                  -> "us-east",
        "aws.credentials.Token.value" -> "token",
        "port"                        -> "10",
        "default"                     -> "12",
        "dburl.value"                 -> "some url",
        "amount"                      -> "3.14",
        "quanity"                     -> "30.0",
        "price.Inr.value"             -> "1000",
        "anotherDefault"              -> "14"
      ),
      pathDelim = "."
    )

  private val config = source.load(automaticConfig)

  // TODO; Not working for option
  assert(
    config equalM
      MyConfig(Aws("us-east", Token("token")), Inr(1000), DbUrl("some url"), 10, 3.14, 30.0, 12, 14)
  )

}
