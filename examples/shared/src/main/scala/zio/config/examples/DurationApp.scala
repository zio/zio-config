package zio.config.examples

import zio.{ConfigProvider, Duration, ZIOAppDefault}
import zio.config.magnolia._
import zio.config.typesafe._

object Example extends ZIOAppDefault {
  case class Foo(duration: Duration)

  val run = ConfigProvider
    .fromHoconString("duration = 10.seconds")
    .load(deriveConfig[Foo])
    .debug
}
