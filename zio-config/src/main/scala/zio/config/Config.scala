package zio.config

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
    source: ConfigSource[K, V],
    configDescriptor: ConfigDescriptor[K, V, A]
  ): IO[ReadErrors[Vector[K], V], Config[A]] =
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

  def fromEnv[K, V, A](
    configDescriptor: ConfigDescriptor[String, String, A]
  ): IO[ReadErrors[Vector[String], String], Config[A]] =
    make(ConfigSource.fromEnv, configDescriptor)

  def fromMap[A](
    map: Map[String, String],
    configDescriptor: ConfigDescriptor[String, String, A]
  ): IO[ReadErrors[Vector[String], String], Config[A]] =
    make[String, String, A](ConfigSource.fromMap(map), configDescriptor)

  def fromPropertyFile[K, V, A](
    configDescriptor: ConfigDescriptor[String, String, A]
  ): ZIO[System, ReadErrors[Vector[String], String], Config[A]] =
    make(ConfigSource.fromProperty, configDescriptor)
}
