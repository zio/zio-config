package zio.config

import java.util.Properties

import zio.system.System
import zio.{ Layer, Tagged, ZIO, ZLayer }
import java.util.Properties

object Config {
  def fromMap[A](
    map: Map[String, String],
    configDescriptor: ConfigDescriptor[String, String, A],
    source: String = "constant",
    keyDelimiter: Char = '.',
    valueDelimiter: Option[Char] = None
  )(implicit tagged: Tagged[A]): Layer[ReadError[String], Config[A]] =
    fromConfigDescriptor(configDescriptor from ConfigSource.fromMap(map, source, keyDelimiter, valueDelimiter))

  def fromMultiMap[A](
    map: Map[String, ::[String]],
    configDescriptor: ConfigDescriptor[String, String, A],
    source: String,
    keyDelimiter: Char = '.'
  )(implicit tagged: Tagged[A]): Layer[ReadError[String], Config[A]] =
    fromConfigDescriptor(configDescriptor from ConfigSource.fromMultiMap(map, source, keyDelimiter))

  def fromProperties[A](
    properties: Properties,
    configDescriptor: ConfigDescriptor[String, String, A],
    source: String,
    keyDelimiter: Char = '.',
    valueDelimiter: Option[Char] = None
  )(implicit tagged: Tagged[A]): Layer[ReadError[String], Config[A]] =
    fromConfigDescriptor(
      configDescriptor from ConfigSource.fromProperties(properties, source, keyDelimiter, valueDelimiter)
    )

  def fromPropertiesFile[A](
    filePath: String,
    configDescriptor: ConfigDescriptor[String, String, A],
    keyDelimiter: Char = '.',
    valueDelimiter: Option[Char] = None
  )(implicit tagged: Tagged[A]): Layer[Throwable, Config[A]] =
    fromConfigDescriptorM(
      ConfigSource
        .fromPropertiesFile(filePath, keyDelimiter, valueDelimiter)
        .map(configDescriptor from _)
    )

  def fromSystemEnv[K, V, A](
    configDescriptor: ConfigDescriptor[String, String, A],
    valueDelimiter: Option[Char] = None
  )(implicit tagged: Tagged[A]): Layer[ReadError[String], Config[A]] =
    fromConfigDescriptorM(ConfigSource.fromSystemEnv(valueDelimiter).map(configDescriptor from _))

  def fromSystemProperties[K, V, A](
    configDescriptor: ConfigDescriptor[String, String, A],
    valueDelimiter: Option[Char] = None
  )(implicit tagged: Tagged[A]): ZLayer[System, ReadError[String], Config[A]] =
    fromConfigDescriptorM(ConfigSource.fromSystemProperties(valueDelimiter).map(configDescriptor from _))

  private[config] def fromConfigDescriptor[K, V, A](
    configDescriptor: ConfigDescriptor[K, V, A]
  )(implicit tagged: Tagged[A]): Layer[ReadError[K], Config[A]] =
    ZLayer.fromEffect(ZIO.fromEither(read(configDescriptor)))

  private[config] def fromConfigDescriptorM[R, E >: ReadError[K], K, V, A](
    configDescriptor: ZIO[R, E, ConfigDescriptor[K, V, A]]
  )(implicit tagged: Tagged[A]): ZLayer[R, E, Config[A]] =
    ZLayer.fromEffect(
      configDescriptor.flatMap(
        descriptor => ZIO.fromEither(read(descriptor))
      )
    )

}
