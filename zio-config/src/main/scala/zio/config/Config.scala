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

  def make[K, V, A](
    source: ConfigSource[Vector[K], V],
    configDescriptor: ConfigDescriptor[K, V, A]
  ): IO[ReadErrors[K, V], Config[A]] =
    Read
      .read(configDescriptor from source)
      .map(
        e =>
          new Config[A] {
            override def config: Service[A] = new Service[A] {
              override def config: UIO[A] = ZIO.succeed(e)
            }
          }
      )

  def fromEnv[K, V, A](configDescriptor: ConfigDescriptor[String, String, A]): IO[ReadErrors[String, String], Config[A]] =
    make(ConfigSource.fromEnv, configDescriptor)

  def fromMap[A](
    map: Map[String, String],
    configDescriptor: ConfigDescriptor[String, String, A]
  ): IO[ReadErrors[String, String], Config[A]] =
    make[String, String, A](ConfigSource.fromMap(map), configDescriptor)

  def fromPropertyFile[K, V, A](configDescriptor: ConfigDescriptor[String, String, A]): ZIO[System, ReadErrors[String, String], Config[A]] =
    make(ConfigSource.fromProperty, configDescriptor)
}
