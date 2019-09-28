package zio.config

import zio.{ Task, UIO }

trait Sources {
  def envSource: Task[ConfigSource] =
    Task(sys.env).map(mapSource)

  def propSource: Task[ConfigSource] =
    Task(sys.props.toMap).map(mapSource)

  def mapSource(map: Map[String, String]): ConfigSource =
    new ConfigSource {
      def configService: ConfigSource.Service = (path: String) => UIO(map.get(path))
    }

  // TODO HOCON etc as separate modules
}
