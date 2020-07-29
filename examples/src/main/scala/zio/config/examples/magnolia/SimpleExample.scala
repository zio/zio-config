package zio.config.examples.magnolia

import zio.config.magnolia.describe
import zio.config.examples.magnolia.MyConfig._
import zio.config.magnolia.DeriveConfigDescriptor._
import zio.config._

final case class MyConfig(
  aws: Aws,
  price: Price,
  dburl: DbUrl,
  port: Int,
  amount: Option[Double],
  quanity: Either[Double, String],
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

  @describe("This config is about aws")
  final case class Aws(region: String, credentials: Credentials)
  final case class DbUrl(value: String) extends AnyVal
}

object AutomaticConfigDescriptor extends App {
  private val automaticConfig = descriptor[MyConfig]

  private val source =
    ConfigSource.fromMap(
      Map(
        "aws.region"                  -> "us-east",
        "aws.credentials.token.value" -> "token",
        "port"                        -> "10",
        "default"                     -> "12",
        "dburl.value"                 -> "some url",
        "amount"                      -> "3.14",
        "quanity"                     -> "30.0",
        "price.inr.value"             -> "1000",
        "anotherDefault"              -> "14"
      ),
      keyDelimiter = Some('.')
    )

  private val config = read(automaticConfig from source)

  println(config)

  assert(
    config ==
      Right(MyConfig(Aws("us-east", Token("token")), Inr(1000), DbUrl("some url"), 10, Some(3.14), Left(30.0), 12, 14))
  )

}
