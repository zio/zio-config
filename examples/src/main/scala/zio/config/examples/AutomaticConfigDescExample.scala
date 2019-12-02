package zio.config.examples

import zio.ZIO
import zio.config.ConfigSource
import zio.config.magnolia.ConfigDescriptorProvider._
import zio.console.Console.Live.console._

final case class MyConfig(port: Int, dburl: String, port2: Option[Double])

object AutomaticConfigDescriptor extends zio.App {
  // Typeclass derivation through Magnolia
  private val configDesc = description[MyConfig]

  private val source =
    ConfigSource.fromMap(
      Map("port" -> "1", "dburl" -> "some url", "port2" -> "3.14")
    )

  private val config = configDesc from source

  import zio.config._

  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    read(config).foldM(
      r => putStrLn(r.mkString(",")) *> ZIO.succeed(1),
      result => putStrLn(result.toString) *> ZIO.succeed(0)
    )
  //
  // SimpleConfig(3,some url,1)
  //
  // Process finished with exit code 0
  //
  //
}
