package zio.config.examples

import zio.ZIO
import zio.config.ConfigSource
import zio.config.magnolia.describe
import zio.config.magnolia.ConfigDescriptorProvider._
import zio.console.Console.Live.console._
import MyConfig._

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
  case class Password(password: String) extends Credentials
  case class Token(token: String)       extends Credentials

  sealed trait Price
  case class INR(inr: Int)        extends Price
  case class AUD(dollars: Double) extends Price

  @describe("This config is about aws")
  final case class Aws(region: String, credentials: Credentials)

  final case class DbUrl(value: String) extends AnyVal
}

object AutomaticConfigDescriptor extends zio.App {
  // Typeclass derivation through Magnolia
  private val automaticConfig = description[MyConfig]

  private val source =
    ConfigSource.fromMap(
      Map(
        "aws.region"            -> "us-east",
        "aws.credentials.token" -> "password",
        "port"                  -> "10",
        "default"               -> "12",
        "dburl.value"           -> "some url",
        "dburl"                 -> "some url",
        "amount"                -> "3.14",
        "quanity"               -> "30.0",
        "price.inr"             -> "1000",
        "anotherDefault"        -> "14"
      )
    )

  private val config = automaticConfig from source

  import zio.config._

  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    read(config).foldM(
      r => putStrLn(r.mkString(",")) *> ZIO.succeed(1),
      result =>
        putStrLn(result.toString()) *> putStrLn(write(config, result).toString()) *> putStrLn(
          generateDocs(config).toString()
        ) *>
          ZIO.succeed(0)
    )
  //
  // Read output:
  //=============
  //  MyConfig(
  //    Aws(us-east, Token(some token)),
  //    Currency(50.0),
  //    DbUrl(some url),
  //    1,
  //    Some(3.14),
  //    Right(30 kilos),
  //    1,
  //    3
  //  )
  //
  // Write output:
  // =============
  // Right(
  //  Record(
  //    HashMap(
  //      anotherDefault -> Leaf(3),
  //      aws ->
  //        Record(
  //          HashMap(
  //            token -> Leaf(some token),
  //            region -> Leaf(us-east)
  //          )
  //        ),
  //      port2 -> Leaf(3.14),
  //      price -> Leaf(30 euros),
  //      default -> Leaf(1),
  //      dburl -> Leaf(some url),
  //      price2 -> Leaf(50.0),
  //      port -> Leaf(1)
  //    )
  //  )
  //
  //
  //
  // Process finished with exit code 0
  //
}
