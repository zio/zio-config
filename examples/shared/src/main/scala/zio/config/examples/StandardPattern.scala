package zio.config.examples

import zio.{Config, Runtime, Scope, ZIO, ZIOAppArgs, ZLayer}
import zio.config.magnolia.deriveConfig
import zio.config.typesafe.TypesafeConfigProvider

object StandardPattern extends zio.ZIOAppDefault {

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

  override def run: ZIO[Environment with ZIOAppArgs with Scope, Any, Any] =
    ZIO
      .config(CombinedArrayValue.config)
      .debug("result")

}
