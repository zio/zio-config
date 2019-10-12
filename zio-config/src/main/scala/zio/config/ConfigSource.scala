package zio.config

import zio.IO

trait ConfigSource { self =>
  val configService: ConfigSource.Service

  final def orElse(that: ConfigSource): ConfigSource =
    new ConfigSource {
      val configService: ConfigSource.Service =
        new ConfigSource.Service {
          val sourceDescription: String = {
            val selfDesc = self.configService.sourceDescription
            val thatDesc = that.configService.sourceDescription

            s"${selfDesc}: with fallback source: ${thatDesc} (on all errors)"
          }

          def getConfigValue(path: String): IO[ReadError, String] =
            self.configService
              .getConfigValue(path)
              .orElse(
                that.configService
                  .getConfigValue(path)
              )
        }
    }

  final def catchSome(
    pf: PartialFunction[ReadError, ConfigSource],
    fallbackSource: String,
    fallbackDescription: String
  ): ConfigSource =
    new ConfigSource {
      val configService: ConfigSource.Service =
        new ConfigSource.Service {
          val sourceDescription: String = {
            val selfDesc = self.configService.sourceDescription

            s"${selfDesc}: with fallback source: ${fallbackSource} (${fallbackDescription})"
          }

          def getConfigValue(path: String): IO[ReadError, String] =
            self.configService
              .getConfigValue(path)
              .catchSome {
                pf.andThen(
                  _.configService
                    .getConfigValue(path)
                )
              }
        }
    }

  final def ifMissingValue(that: => ConfigSource): ConfigSource =
    catchSome(
      { case ReadError.MissingValue(_) => that },
      that.configService.sourceDescription,
      "on missing value"
    )
}

object ConfigSource {
  trait Service {
    val sourceDescription: String

    def getConfigValue(path: String): IO[ReadError, String]
  }
}
