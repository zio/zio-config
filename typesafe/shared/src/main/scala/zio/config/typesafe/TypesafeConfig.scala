package zio.config.typesafe

import java.io.File

import com.typesafe.config.ConfigFactory
import zio.config._
import zio.{ Has, Layer, Tag, ZIO }

object TypesafeConfig {

  /**
   * Retrieve your config from a given file in resource classpath, that is following HOCON format.
   * A simple key value file with the name sufficed by `.properties` will work.
   *
   * A complete example usage:
   *
   * {{{
   *
   *   import zio.config.magnolia.DeriveConfigDescriptor.descriptor
   *
   *   case class MyConfig(port: Int, url: String)
   *
   *   val result: Layer[ReadError[String], Has[MyConfig]] =
   *     TypesafeConfig.fromDefaultLoader(descriptor[MyConfig])
   * }}}
   */
  def fromDefaultLoader[A](
    configDescriptor: ConfigDescriptor[A]
  )(implicit tag: Tag[A]): Layer[ReadError[String], Has[A]] =
    fromTypesafeConfig(ConfigFactory.load.resolve, configDescriptor)

  /**
   * Retrieve your config from a HOCON file
   *
   * A complete example usage:
   *
   * {{{
   *
   *   import zio.config.magnolia.DeriveConfigDescriptor.descriptor
   *
   *   case class MyConfig(port: Int, url: String)
   *
   *   val result: Layer[ReadError[String], Has[MyConfig]] =
   *     TypesafeConfig.fromHoconFile(new File("/path/to/xyz.hocon"), descriptor[MyConfig])
   * }}}
   */
  /*
  def fromHoconFile[A](file: File, configDescriptor: ConfigDescriptor[A])(
    implicit tag: Tag[A]
  ): Layer[ReadError[String], Has[A]] =
    fromTypesafeConfig(ConfigFactory.parseFile(file).resolve, configDescriptor)
   */
  /**
   * Retrieve a config from `typesafe-config` from a given Hocon File.
   *
   * A complete example usage:
   *
   * {{{
   *
   *   import zio.config.magnolia.DeriveConfigDescriptor.descriptor
   *
   *   case class MyConfig(port: Int, url: String)
   *
   *   val result: Layer[ReadError[String], Has[MyConfig]] =
   *     TypesafeConfig.fromHoconFile(new File("/path/to/xyz.hocon"), descriptor[MyConfig])
   * }}}
   */
  def fromHoconString[A](str: String, configDescriptor: ConfigDescriptor[A])(
    implicit tag: Tag[A]
  ): Layer[ReadError[String], Has[A]] =
    fromTypesafeConfig(ConfigFactory.parseString(str).resolve, configDescriptor)

  /**
   * Retrieve a config from `com.typesafe.config.Config`
   *
   * A complete example usage:
   *
   * {{{
   *
   *   import zio.config.magnolia.DeriveConfigDescriptor.descriptor
   *
   *   case class MyConfig(port: Int, url: String)
   *
   *   val result: Layer[ReadError[String], Has[MyConfig]] =
   *     TypesafeConfig.fromHoconFile(ConfigFactory.load.resolve, descriptor[MyConfig])
   * }}}
   */
  def fromTypesafeConfig[A](
    conf: => com.typesafe.config.Config,
    configDescriptor: ConfigDescriptor[A]
  )(implicit tag: Tag[A]): Layer[ReadError[String], Has[A]] =
    ZConfig.fromConfigDescriptorM(
      ZIO
        .fromEither(TypesafeConfigSource.fromTypesafeConfig(conf))
        .map(configDescriptor from _)
    )
}
