package zio.config.examples

import zio.ZIO
import zio.config.ConfigSource
import zio.config.magnolia.ConfigDescriptorProvider._
import zio.console.Console.Live.console._

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
  quanity: Either[Double, String],
  default: Int = 1,
  anotherDefault: Int = 2
)

object AutomaticConfigDescriptor extends zio.App {
  // Typeclass derivation through Magnolia
  private val configDesc = description[MyConfig]

  private val source =
    ConfigSource.fromMap(
      Map(
        "aws.region"            -> "us-east",
        "aws.credentials.token" -> "some token",
        "port"                  -> "1",
        "dburl"                 -> "some url",
        "amount"                -> "3.14",
        "quanity"               -> "30 kilos",
        "price.dollars"         -> "50",
        "anotherDefault"        -> "3"
      )
    )

  private val config = configDesc from source

  import zio.config._

  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    read(config).foldM(
      r => putStrLn(r.mkString(",")) *> ZIO.succeed(1),
      result => putStrLn(result.toString()) *> putStrLn(write(config, result).toString()) *> ZIO.succeed(0)
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
  // Process finished with exit code 0
  //
}
