package zio.config.examples

import zio.ZIO
import zio.config.ConfigSource
import zio.config.magnolia.ConfigDescriptorProvider._
import zio.console.Console.Live.console._

final case class Token(value: String) extends AnyVal
final case class Aws(region: String, token: Token)
final case class MyConfig(
  aws: Aws,
  port: Int,
  dburl: String,
  port2: Option[Double],
  price: Either[Double, String],
  price2: Either[Double, String],
  default: Int = 1,
  anotherDefault: Int = 2
)

object AutomaticConfigDescriptor extends zio.App {
  // Typeclass derivation through Magnolia
  private val configDesc = description[MyConfig]

  private val source =
    ConfigSource.fromMap(
      Map(
        "aws.region"     -> "us-east",
        "aws.token"      -> "some token",
        "port"           -> "1",
        "dburl"          -> "some url",
        "port2"          -> "3.14",
        "price"          -> "30 euros",
        "price2"         -> "50",
        "anotherDefault" -> "3"
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
  // MyConfig(Aws(us-east,Token(some token)),1,some url,Some(3.14),Right(30 euros),Left(50.0),1,3)
  //
  // Process finished with exit code 0
  //
  //
}
