package zio.config

import zio.Task

trait ConfigSource {
  val configService: ConfigSource.Service
}

object ConfigSource {
  trait Service {
    def getConfigValue(path: String): Task[String]
  }
}
