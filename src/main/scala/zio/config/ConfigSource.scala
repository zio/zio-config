package zio.config

import zio.UIO

trait ConfigSource {
  def configService: ConfigSource.Service
}

object ConfigSource {
  trait Service {
    def getString(path: String): UIO[Option[String]]
  }
}
