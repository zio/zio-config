package zio.config.examples.typesafe

import zio.{ Config, ConfigProvider, Unsafe}
import zio.config.typesafe._
import zio.config.magnolia._

object LoggerExample extends App {

  val hocon =
    s"""
       | duration = "10.seconds"
       |""".stripMargin

  final case class 
  val pgm2 = ConfigProvider.fromHoconString(hocon).load(deriveConfig[])

  println(Unsafe.unsafe { implicit u =>
    zio.Runtime.default.unsafe
      .run(
        pgm2
      )
      .getOrThrowFiberFailure()
  })

  // LoggingConfig(%timestamp{yyyy-MM-dd'T'HH:mm:ssZ} %level [%fiberId] %name:%line %message %cause,file:///tmp/console_app.log,FilterConfig(DEBUG,Map(abc -> LogLevel(10000,DEBUG,7), zio.logging.example.LivePingService -> LogLevel(10000,DEBUG,7))))
}
