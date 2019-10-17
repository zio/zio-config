package zio.config

import zio.config.ReadErrors.ReadError
import zio.system.System
import zio.{ IO, UIO, ZIO }

trait ConfigSource[K, V] { self =>
  val configSourceService: ConfigSource.Service[K, V]

  final def orElse(that: ConfigSource[K, V]): ConfigSource[K, V] =
    new ConfigSource[K, V] {
      val configSourceService: ConfigSource.Service[K, V] =
        new ConfigSource.Service[K, V] {
          val sourceDescription: String = {
            val selfDesc = self.configSourceService.sourceDescription
            val thatDesc = that.configSourceService.sourceDescription

            s"${selfDesc}: with fallback source: ${thatDesc} (on all errors)"
          }

          def getConfigValue(path: Vector[K]): IO[ReadError[K, V], ConfigSource.Value[V]] =
            self.configSourceService
              .getConfigValue(path)
              .orElse(
                that.configSourceService
                  .getConfigValue(path)
              )
        }
    }

  final def <>(that: ConfigSource[K, V]): ConfigSource[K, V] =
    orElse(that)

  final def catchMissingValue(that: => ConfigSource[K, V]): ConfigSource[K, V] =
    new ConfigSource[K, V] {
      val configSourceService: ConfigSource.Service[K, V] =
        new ConfigSource.Service[K, V] {
          val sourceDescription: String = {
            val selfDesc = self.configSourceService.sourceDescription
            val thatDesc = that.configSourceService.sourceDescription

            s"${selfDesc}: with fallback source: ${thatDesc} (on missing value)"
          }

          def getConfigValue(path: Vector[K]): IO[ReadError[K, V], ConfigSource.Value[V]] =
            self.configSourceService
              .getConfigValue(path)
              .catchSome {
                case ReadError.MissingValue(_) =>
                  that.configSourceService
                    .getConfigValue(path)
              }
        }
    }

  final def <>?(that: ConfigSource[K, V]): ConfigSource[K, V] =
    catchMissingValue(that)

  final def map[V2](f: V => V2): ConfigSource[K, V2] =
    new ConfigSource[K, V2] {
      val configSourceService: ConfigSource.Service[K, V2] =
        new ConfigSource.Service[K, V2] {
          val sourceDescription: String =
            self.configSourceService.sourceDescription

          def getConfigValue(path: Vector[K]): IO[ReadError[K, V2], ConfigSource.Value[V2]] =
            self.configSourceService
              .getConfigValue(path)
              .foldM(
                {
                  case ReadError.ParseError(key, provided, message) =>
                    ZIO.fail(ReadError.ParseError(key, f(provided), message))
                  case missing @ ReadError.MissingValue(_) => ZIO.fail(missing)
                  case fatal @ ReadError.FatalError(_, _)  => ZIO.fail(fatal)
                },
                v => ZIO.succeed(ConfigSource.Value(f(v.value), v.source))
              )
        }
    }
}

object ConfigSource {
  final case class Value[V](value: V, source: String)

  trait Service[K, V] {
    val sourceDescription: String

    def getConfigValue(path: Vector[K]): IO[ReadError[K, V], Value[V]]
  }

  val fromEnv: ZIO[System, Nothing, ConfigSource[String, String]] =
    ZIO.access { env =>
      new ConfigSource[String, String] {
        val configSourceService: ConfigSource.Service[String, String] =
          new ConfigSource.Service[String, String] {
            val sourceDescription: String = "Environment"

            def getConfigValue(path: Vector[String]): IO[ReadError[String, String], ConfigSource.Value[String]] = {
              val key = path.mkString("_")

              env.system
                .env(key)
                .mapError { err =>
                  ReadError.FatalError(key, err)
                }
                .flatMap {
                  IO.fromOption(_)
                    .foldM(
                      _ => IO.fail(ReadError.MissingValue(key)),
                      value => UIO(ConfigSource.Value(value, sourceDescription))
                    )
                }
            }
          }
      }
    }

  val fromProperty: ZIO[System, Nothing, ConfigSource[String, String]] =
    ZIO.access { env =>
      new ConfigSource[String, String] {
        val configSourceService: ConfigSource.Service[String, String] =
          new ConfigSource.Service[String, String] {
            val sourceDescription: String = "System properties"

            def getConfigValue(path: Vector[String]): IO[ReadError[String, String], ConfigSource.Value[String]] = {
              val key = path.mkString(".")

              env.system
                .property(key)
                .mapError { err =>
                  ReadError.FatalError(key, err)
                }
                .flatMap {
                  IO.fromOption(_)
                    .foldM(
                      _ => IO.fail(ReadError.MissingValue(key)),
                      value => UIO(ConfigSource.Value(value, sourceDescription))
                    )
                }
            }
          }
      }
    }

  def fromMap(m: Map[String, String], delimiter: String = "."): ConfigSource[String, String] =
    new ConfigSource[String, String] {
      val configSourceService: ConfigSource.Service[String, String] =
        new ConfigSource.Service[String, String] {
          val sourceDescription: String = "Scala Map"

          def getConfigValue(path: Vector[String]): IO[ReadError[String, String], ConfigSource.Value[String]] = {
            val key = path.mkString(delimiter)

            ZIO
              .fromOption(m.get(key))
              .foldM(
                _ => IO.fail(ReadError.MissingValue(key)),
                value => UIO(ConfigSource.Value(value, sourceDescription))
              )
          }
        }
    }

  def fromPropertyTree(
    propertyTree: PropertyTree[String, String],
    delimiter: String = "."
  ): ConfigSource[String, String] =
    fromMap(propertyTree.flattenString(delimiter))
}
