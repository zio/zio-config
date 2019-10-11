package zio.config

import zio.IO

trait ConfigSource {
  val configService: ConfigSource.Service
}

object ConfigSource {
  trait Service {
    val sourceDescription: String

    def getConfigValue(path: String): IO[ReadError, String]
  }

  implicit class ConfigSourceOps(private val source: ConfigSource) extends AnyVal {
    def orElse(that: ConfigSource): ConfigSource =
      new ConfigSource {
        val configService: ConfigSource.Service =
          new Service {
            val sourceDescription: String = {
              val sourceDesc = source.configService.sourceDescription
              val thatDesc   = that.configService.sourceDescription

              s"${sourceDesc}: with fallback source: ${thatDesc} (on all errors)"
            }

            def getConfigValue(path: String): IO[ReadError, String] =
              source.configService
                .getConfigValue(path)
                .orElse(
                  that.configService
                    .getConfigValue(path)
                )
          }
      }

    def catchSome(
      pf: PartialFunction[ReadError, ConfigSource],
      fallbackSource: String,
      fallbackDescription: String
    ): ConfigSource =
      new ConfigSource {
        val configService: ConfigSource.Service =
          new Service {
            val sourceDescription: String = {
              val sourceDesc = source.configService.sourceDescription

              s"${sourceDesc}: with fallback source: ${fallbackSource} (${fallbackDescription})"
            }

            def getConfigValue(path: String): IO[ReadError, String] =
              source.configService
                .getConfigValue(path)
                .catchSome {
                  pf.andThen(
                    _.configService
                      .getConfigValue(path)
                  )
                }
          }
      }

    def ifMissingValue(that: => ConfigSource): ConfigSource =
      catchSome(
        { case ReadError.MissingValue(_) => that },
        that.configService.sourceDescription,
        "on missing value"
      )
  }
}
