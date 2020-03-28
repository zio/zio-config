package zio.config

import java.io.{ File, FileInputStream }
import java.util.Properties
import zio.system.System.Live.system.lineSeparator
import zio.{ IO, Task, ZIO }

trait Config[A] { self =>
  def config: Config.Service[A]
}
object Config {
  trait Service[A] {
    def config: A
  }

  def make[K, V, A](
    source: ConfigSource[K, V],
    configDescriptor: ConfigDescriptor[K, V, A]
  ): IO[ReadError[K], Config[A]] =
    IO.fromEither(read(configDescriptor from source))
      .map(
        a =>
          new Config[A] {
            override def config: Service[A] =
              new Service[A] {
                override def config: A = a
              }
          }
      )

  def fromEnv[A](
    configDescriptor: ConfigDescriptor[String, String, A],
    valueDelimiter: Option[Char] = None
  ): IO[ReadError[String], Config[A]] =
    ConfigSource
      .fromSystemEnv(valueDelimiter)
      .flatMap((r: ConfigSource[String, String]) => make[String, String, A](r, configDescriptor))

  def fromMap[A](
    map: Map[String, String],
    configDescriptor: ConfigDescriptor[String, String, A],
    pathDelimiter: Char = '.'
  ): IO[ReadError[String], Config[A]] =
    make[String, String, A](ConfigSource.fromMap(map, pathDelimiter), configDescriptor)

  def fromMultiMap[A](
    map: Map[String, ::[String]],
    configDescriptor: ConfigDescriptor[String, String, A],
    pathDelimiter: Char = '.'
  ): IO[ReadError[String], Config[A]] =
    make[String, String, A](ConfigSource.fromMultiMap(map, pathDelimiter), configDescriptor)

  // If reading a file, this can have read errors as well as throwable when trying to read the file
  def fromPropertyFile[A](
    filePath: String,
    configDescriptor: ConfigDescriptor[String, String, A]
  ): Task[Config[A]] =
    ZIO
      .bracket(ZIO.effect(new FileInputStream(new File(filePath))))(r => ZIO.effectTotal(r.close()))(inputStream => {
        ZIO.effect {
          val properties = new Properties()
          properties.load(inputStream)
          properties
        }
      })
      .flatMap(
        properties =>
          lineSeparator.flatMap(
            ln =>
              make(ConfigSource.fromProperties(properties), configDescriptor)
                .mapError(r => new RuntimeException(s"${ln}${r}"))
          )
      )

  def fromPropertyFile[A](
    configDescriptor: ConfigDescriptor[String, String, A],
    valueDelimiter: Option[Char] = None
  ): IO[ReadError[String], Config[A]] =
    ConfigSource.fromSystemProperties(valueDelimiter).flatMap(r => make(r, configDescriptor))
}
