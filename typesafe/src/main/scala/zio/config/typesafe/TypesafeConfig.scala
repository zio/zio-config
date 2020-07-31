package zio.config.typesafe

import java.io.File

import com.typesafe.config.ConfigFactory
import zio.config.{ ConfigDescriptor, ReadError, ZConfig }
import zio.{ Layer, Tag, ZIO }

object TypesafeConfig {
  def fromDefaultLoader[A](
    configDescriptor: ConfigDescriptor[A]
  )(implicit tag: Tag[A]): Layer[ReadError[String], ZConfig[A]] =
    fromTypesafeConfig(ConfigFactory.load.resolve, configDescriptor)

  def fromHoconFile[A](
    file: File,
    configDescriptor: ConfigDescriptor[A]
  )(implicit tag: Tag[A]): Layer[ReadError[String], ZConfig[A]] =
    fromTypesafeConfig(ConfigFactory.parseFile(file).resolve, configDescriptor)

  def fromHoconString[A](
    str: String,
    configDescriptor: ConfigDescriptor[A]
  )(implicit tag: Tag[A]): Layer[ReadError[String], ZConfig[A]] =
    fromTypesafeConfig(ConfigFactory.parseString(str).resolve, configDescriptor)

  def fromTypesafeConfig[A](
    conf: => com.typesafe.config.Config,
    configDescriptor: ConfigDescriptor[A]
  )(implicit tag: Tag[A]): Layer[ReadError[String], ZConfig[A]] =
    ZConfig.fromConfigDescriptorM(
      ZIO
        .fromEither(TypesafeConfigSource.fromTypesafeConfig(conf))
        .map(configDescriptor from _)
    )
}
