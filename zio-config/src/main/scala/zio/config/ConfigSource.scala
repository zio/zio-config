package zio.config

import zio.IO

trait ConfigSource {
  val configService: ConfigSource.Service
}

object ConfigSource {
  trait Service {
    def getConfigValue(path: String): IO[ReadError, String]
  }
}
