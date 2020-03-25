package zio.config

import java.io.{ File, FileInputStream }
import java.util.Properties

import zio.system.System
import zio.{ system, IO, Tagged, ZEnv, ZIO, ZLayer }

object Config {

  trait Service[A] {
    def config: A
  }

  def succeed[A](a: A): Service[A] = new Service[A] {
    def config: A = a
  }

  def make[K, V, A](
    source: ConfigSource[K, V],
    configDescriptor: ConfigDescriptor[K, V, A]
  )(implicit tagged: Tagged[Service[A]]): ZLayer.NoDeps[ReadErrors[Vector[K], V], Config[A]] =
    ZLayer.fromEffect(makeM(source, configDescriptor))

  def makeM[K, V, A](
    source: ConfigSource[K, V],
    configDescriptor: ConfigDescriptor[K, V, A]
  ): IO[ReadErrors[Vector[K], V], Service[A]] =
    read(configDescriptor from source).map(succeed)

  def fromEnv[K, V, A](
    configDescriptor: ConfigDescriptor[String, String, A],
    valueDelimiter: Option[String] = None
  )(implicit tagged: Tagged[Service[A]]): ZLayer.NoDeps[ReadErrors[Vector[String], String], Config[A]] =
    make(ConfigSource.fromEnv(valueDelimiter), configDescriptor)

  def fromMap[A](
    map: Map[String, String],
    configDescriptor: ConfigDescriptor[String, String, A],
    pathDelimiter: String = "."
  )(implicit tagged: Tagged[Service[A]]): ZLayer.NoDeps[ReadErrors[Vector[String], String], Config[A]] =
    make[String, String, A](ConfigSource.fromMap(map, pathDelimiter), configDescriptor)

  def fromMultiMap[A](
    map: Map[String, ::[String]],
    configDescriptor: ConfigDescriptor[String, String, A],
    pathDelimiter: String = "."
  )(implicit tagged: Tagged[Service[A]]): ZLayer.NoDeps[ReadErrors[Vector[String], String], Config[A]] =
    make[String, String, A](ConfigSource.fromMultiMap(map, pathDelimiter), configDescriptor)

  // If reading a file, this can have read errors as well as throwable when trying to read the file
  def fromPropertyFile[A](
    filePath: String,
    configDescriptor: ConfigDescriptor[String, String, A]
  )(implicit tagged: Tagged[Service[A]]): ZLayer.NoDeps[Throwable, Config[A]] =
    ZLayer.fromEffect(
      for {
        properties <- ZIO.bracket(ZIO.effect(new FileInputStream(new File(filePath))))(r => ZIO.effectTotal(r.close()))(
                       inputStream => {
                         ZIO.effect {
                           val properties = new Properties()
                           properties.load(inputStream)
                           properties
                         }
                       }
                     )
        ln <- system.lineSeparator.provideLayer(ZEnv.live)
        config <- makeM(ConfigSource.fromJavaProperties(properties), configDescriptor)
                   .mapError(r => new RuntimeException(s"${ln}${r.mkString(ln)}"))
      } yield config
    )

  def fromPropertyFile[K, V, A](
    configDescriptor: ConfigDescriptor[String, String, A],
    valueDelimiter: Option[String] = None
  )(implicit tagged: Tagged[Service[A]]): ZLayer[System, ReadErrors[Vector[String], String], Config[A]] =
    make(ConfigSource.fromProperty(valueDelimiter), configDescriptor)
}
