package zio.config.examples

import zio.ZIO
import zio.config.ConfigSource
import zio.config.magnolia.ConfigDescriptorProvider._
import zio.console.Console.Live.console._

final case class Aws(region: String)
final case class MyConfig(
  port: Int,
  dburl: String,
  port2: Option[Double],
  price: Either[Double, String],
  price2: Either[Double, String],
  aws: Aws,
  default: Int = 1,
  anotherDefault: Int = 2
)

object AutomaticConfigDescriptor extends zio.App {
  // Typeclass derivation through Magnolia
  private val configDesc = description[MyConfig]

  private val source =
    ConfigSource.fromMap(
      Map(
        "port"           -> "1",
        "dburl"          -> "some url",
        "port2"          -> "3.14",
        "price"          -> "30 euros",
        "price2"         -> "50",
        "region"         -> "us-east",
        "anotherDefault" -> "3"
      )
    )

  private val config = configDesc from source

  import zio.config._

  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    read(config).foldM(
      r => putStrLn(r.mkString(",")) *> ZIO.succeed(1),
      result => putStrLn(result.toString) *> ZIO.succeed(0)
    )
  //
  // MyConfig(1,some url,Some(3.14),Right(30 euros),Left(50.0),Aws(us-east),1,3)
  //
  // Process finished with exit code 0
  //
  //
}
