package zio.config

import zio.{ Task, UIO, ZIO }

trait Sources {
  def envSource: Task[ConfigSource] =
    ZIO.effect(sys.env).map(mapSource)

  def propSource: Task[ConfigSource] =
    ZIO.effect(sys.props.toMap).map(mapSource)

  def mapSource(map: Map[String, String]): ConfigSource =
    new ConfigSource {
      override def configService: ConfigSource.Service =
        new ConfigSource.Service {
          override def getString(path: String): UIO[Option[String]] = ZIO.effectTotal(map.get(path))
        }
    }

  // TODO HOCON etc as separate modules
}
