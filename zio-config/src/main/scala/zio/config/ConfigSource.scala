package zio.config

import zio.IO

trait ConfigSource {
  val configService: ConfigSource.Service
}

object ConfigSource {
  trait Service {
    def getConfigValue(path: String): IO[ReadError, String]
  }

  implicit class ConfigSourceOps(private val source: ConfigSource) extends AnyVal {
    def orElse(that: => ConfigSource): ConfigSource =
      new ConfigSource {
        val configService: ConfigSource.Service =
          (path: String) =>
            source.configService
              .getConfigValue(path)
              .orElse(
                that.configService
                  .getConfigValue(path)
              )
      }

    def catchSome(pf: PartialFunction[ReadError, ConfigSource]): ConfigSource =
      new ConfigSource {
        val configService: ConfigSource.Service =
          (path: String) =>
            source.configService
              .getConfigValue(path)
              .catchSome {
                pf.andThen(
                  _.configService
                    .getConfigValue(path)
                )
              }
      }

    def ifMissingValue(that: => ConfigSource): ConfigSource =
      catchSome {
        case ReadError.MissingValue(_) => that
      }
  }
}
