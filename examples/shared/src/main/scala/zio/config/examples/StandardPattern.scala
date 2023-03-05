package zio.config.examples

import zio.{Config, Runtime, ZIO, ZLayer}
import zio.config.magnolia.deriveConfig
import zio.config._
import zio.config.typesafe.TypesafeConfigProvider

object TestApp2 extends zio.ZIOAppDefault {

  val config = """arr = ["a", "b", "c"]"""

  case class CombinedArrayValue(arr: List[String])

  object CombinedArrayValue {
    val config: Config[CombinedArrayValue] = deriveConfig[CombinedArrayValue]
  }

  override val bootstrap: ZLayer[Any, Nothing, Unit] =
    Runtime.setConfigProvider(
      TypesafeConfigProvider
        .fromHoconString(config)
        .kebabCase
    )

  def run = ZIO
    .config(CombinedArrayValue.config)
    .debug("result")
    .exitCode
}