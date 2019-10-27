package zio.config

import java.net.URI
import zio.config.actions.Read
import zio.system.System
import zio.{ IO, UIO, ZIO }

trait Config[A] {
  def config: Config.Service[A]
}
object Config {
  trait Service[A] {
    def config: UIO[A]
  }

  def make[A](
    source: ConfigSource[String, String],
    configDescriptor: ConfigDescriptor[A]
  ): IO[ReadErrors[String, String], Config[A]] =
    Read
      .read(configDescriptor)
      .provide(source)
      .map(
        e =>
          new Config[A] {
            override def config: Service[A] = new Service[A] {
              override def config: UIO[A] = ZIO.succeed(e)
            }
          }
      )

  def fromEnv[A](configDescriptor: ConfigDescriptor[A]): ZIO[System, ReadErrors[String, String], Config[A]] =
    for {
      source <- ConfigSource.fromEnv
      res    <- make(source, configDescriptor)
    } yield res

  def fromMap[A](
    map: Map[String, String],
    configDescriptor: ConfigDescriptor[A]
  ): IO[ReadErrors[String, String], Config[A]] =
    make(ConfigSource.fromMap(map), configDescriptor)

  def fromPropertyFile[A](configDescriptor: ConfigDescriptor[A]): ZIO[System, ReadErrors[String, String], Config[A]] =
    for {
      source <- ConfigSource.fromProperty
      res    <- make(source, configDescriptor)
    } yield res
}
