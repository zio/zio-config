package zio.config

import zio.system.System
import zio.{ Task, ZIO }

trait Sources {
  def envSource: ZIO[System, Throwable, ConfigSource] =
    ZIO.access { env =>
      new ConfigSource {
        val configService: ConfigSource.Service =
          (path: String) =>
            env.system.env(path).flatMap { (oStr: Option[String]) =>
              oStr.fold[Task[String]](
                ZIO.fail(new IllegalArgumentException(s"Not defined in system environment: $path"))
              )(ZIO.succeed)
            }
      }
    }

  def propSource: ZIO[System, Throwable, ConfigSource] =
    ZIO.access { env =>
      new ConfigSource {
        val configService: ConfigSource.Service =
          (path: String) =>
            env.system.property(path).flatMap { (oStr: Option[String]) =>
              oStr.fold[Task[String]](
                ZIO.fail(new IllegalArgumentException(s"System property not found for key: $path"))
              )(ZIO.succeed)
            }
      }
    }

  def mapSource(map: Map[String, String]): ConfigSource =
    new ConfigSource {
      val configService: ConfigSource.Service =
        (path: String) => ZIO.effect(map(path))
    }

  // TODO HOCON etc as separate modules
}
