package zio.config.typesafe

import com.typesafe.config.ConfigFactory
import zio.config._
import zio.{IsNotIntersection, Layer, Tag, ZIO}

import java.io.File

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
   *   val result: Layer[ReadError[String], MyConfig] =
   *     TypesafeConfig.fromDefaultLoader(descriptor[MyConfig])
   * }}}
   */
  def fromResourcePath[A: Tag: IsNotIntersection](
    configDescriptor: ConfigDescriptor[A]
  ): Layer[ReadError[String], A] =
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
   *   val result: Layer[ReadError[String], MyConfig] =
   *     TypesafeConfig.fromHoconFile(new File("/path/to/xyz.hocon"), descriptor[MyConfig])
   * }}}
   */
  def fromHoconFile[A: Tag: IsNotIntersection](
    file: File,
    configDescriptor: ConfigDescriptor[A]
  ): Layer[ReadError[String], A] =
    ZConfig.fromConfigDescriptor(
      configDescriptor from TypesafeConfigSource.fromHoconFile(file)
    )

  /**
   * Retrieve your config from a path to HOCON file
   *
   * A complete example usage:
   *
   * {{{
   *
   *   import zio.config.magnolia.DeriveConfigDescriptor.descriptor
   *
   *   case class MyConfig(port: Int, url: String)
   *
   *   val result: Layer[ReadError[String], MyConfig] =
   *     TypesafeConfig.fromHoconFilePath("/path/to/xyz.hocon", descriptor[MyConfig])
   * }}}
   */
  def fromHoconFilePath[A: Tag: IsNotIntersection](
    filePath: String,
    configDescriptor: ConfigDescriptor[A]
  ): Layer[ReadError[String], A] =
    ZConfig.fromConfigDescriptor(
      configDescriptor from TypesafeConfigSource.fromHoconFilePath(filePath)
    )

  /**
   * Retrieve a config from a given Hocon string.
   *
   * A complete example usage:
   *
   * {{{
   *
   *   import zio.config.magnolia.DeriveConfigDescriptor.descriptor
   *
   *   case class MyConfig(port: Int, url: String)
   *
   *   val configString = """port: 10, url: "http://x.y""""
   *
   *   val result: Layer[ReadError[String], MyConfig] =
   *     TypesafeConfig.fromHoconString(configString, descriptor[MyConfig])
   * }}}
   */
  def fromHoconString[A: Tag: IsNotIntersection](
    hoconString: String,
    configDescriptor: ConfigDescriptor[A]
  ): Layer[ReadError[String], A] =
    ZConfig.fromConfigDescriptor(
      configDescriptor from TypesafeConfigSource.fromHoconString(hoconString)
    )

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
   *   val result: Layer[ReadError[String], MyConfig] =
   *     TypesafeConfig.fromTypesafeConfig(ConfigFactory.load.resolve, descriptor[MyConfig])
   * }}}
   */
  def fromTypesafeConfig[A: Tag: IsNotIntersection](
    conf: => com.typesafe.config.Config,
    configDescriptor: ConfigDescriptor[A]
  ): Layer[ReadError[String], A] =
    ZConfig.fromConfigDescriptor(
      configDescriptor from TypesafeConfigSource.fromTypesafeConfig(ZIO.succeed(conf))
    )
}
