package zio.config.examples.typesafe

import zio.config.typesafe.TypesafeConfigProvider
import zio.{Chunk, Config, LogLevel, Unsafe}
import zio.config._
import zio.config.examples.typesafe.LoggerExample.LoggingConfig.FilterConfig

object LoggerExample extends App {

  final case class LoggingConfig(pattern: String, path: String, filter: FilterConfig)

  object LoggingConfig {
    val config: Config[LoggingConfig] =
      Config
        .string("pattern")
        .zip(Config.string("path"))
        .zip(FilterConfig.config.nested("filter"))
        .to[LoggingConfig]
        .nested("logger")

    final case class FilterConfig(rootLevel: String, mappings: Map[String, LogLevel])

    object FilterConfig {
      val config: Config[FilterConfig] =
        Config
          .string("rootLevel")
          .zip(Config.table("mappings", Config.string.mapOrFail(logLevelValue)).withDefault(Map.empty))
          .to[FilterConfig]

    }

  }
  val logConfigJson =
    s"""
       |logger {
       |
       |  pattern = "%timestamp{yyyy-MM-dd'T'HH:mm:ssZ} %level [%fiberId] %name:%line %message %cause"
       |  path = "file:///tmp/console_app.log"
       |
       |  filter {
       |    rootLevel = "DEBUG"
       |
       |    mappings {
       |      abc = "DEBUG"
       |      "zio.logging.example.LivePingService" = "DEBUG"
       |    }
       |  }
       |}
       |
       |""".stripMargin

  val logLevelMapping: Map[String, LogLevel] = Map(
    LogLevel.All.label     -> LogLevel.All,
    LogLevel.Trace.label   -> LogLevel.Trace,
    LogLevel.Debug.label   -> LogLevel.Debug,
    LogLevel.Info.label    -> LogLevel.Info,
    LogLevel.Warning.label -> LogLevel.Warning,
    LogLevel.Error.label   -> LogLevel.Error,
    LogLevel.Fatal.label   -> LogLevel.Fatal,
    LogLevel.None.label    -> LogLevel.None
  )

  def logLevelValue(value: String): Either[Config.Error.InvalidData, LogLevel] =
    logLevelMapping.get(value.toUpperCase) match {
      case Some(l) => Right(l)
      case None    => Left(Config.Error.InvalidData(Chunk.empty, s"Expected a LogLevel, but found ${value}"))
    }

  val configProvider = TypesafeConfigProvider.fromHoconString(logConfigJson)

  println(Unsafe.unsafe { implicit u =>
    zio.Runtime.default.unsafe
      .run(
        configProvider.load(LoggingConfig.config)
      )
      .getOrThrowFiberFailure()
  })

  // LoggingConfig(%timestamp{yyyy-MM-dd'T'HH:mm:ssZ} %level [%fiberId] %name:%line %message %cause,file:///tmp/console_app.log,FilterConfig(DEBUG,Map(abc -> LogLevel(10000,DEBUG,7), zio.logging.example.LivePingService -> LogLevel(10000,DEBUG,7))))
}
