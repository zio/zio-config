package zio.config

import zio.{ IO, Task }

trait Sources {
  def envSource: Task[ConfigSource] =
    Task(sys.env).map(mapSource)

  def propSource: Task[ConfigSource] =
    Task(sys.props.toMap).map(mapSource)

  def mapSource(map: Map[String, String]): ConfigSource =
    new ConfigSource {
      val configService: ConfigSource.Service = (path: String) => IO.fromOption(map.get(path))
    }

  // TODO HOCON etc as separate modules
}
