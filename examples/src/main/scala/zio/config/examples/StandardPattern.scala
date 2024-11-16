package zio.config.examples

import zio.config.magnolia.deriveConfig
import zio.config.typesafe.TypesafeConfigProvider
import zio.{Config, ExitCode, Runtime, URIO, ZIO, ZLayer}

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

  def run: URIO[Any, ExitCode] = ZIO
    .config(CombinedArrayValue.config)
    .debug("result")
    .exitCode
}
