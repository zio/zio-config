package zio.config.typesafe

import com.typesafe.config.ConfigFactory
import zio._
import zio.config._

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
   *   val result: Layer[ReadError[String], Has[MyConfig]] =
   *     TypesafeConfig.fromResourcePath(descriptor[MyConfig])
   * }}}
   */
  def fromResourcePath[A](
    configDescriptor: ConfigDescriptor[A]
  )(implicit tag: Tag[A]): Layer[ReadError[String], A] =
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
  def fromHoconFile[A](file: File, configDescriptor: ConfigDescriptor[A])(implicit
    tag: Tag[A]
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
   *   val result: Layer[ReadError[String], Has[MyConfig]] =
   *     TypesafeConfig.fromHoconFilePath("/path/to/xyz.hocon", descriptor[MyConfig])
   * }}}
   */
  def fromHoconFilePath[A](filePath: String, configDescriptor: ConfigDescriptor[A])(implicit
    tag: Tag[A]
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
   *   val result: Layer[ReadError[String], Has[MyConfig]] =
   *     TypesafeConfig.fromHoconString(configString, descriptor[MyConfig])
   * }}}
   */
  def fromHoconString[A](hoconString: String, configDescriptor: ConfigDescriptor[A])(implicit
    tag: Tag[A]
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
   *   val result: Layer[ReadError[String], Has[MyConfig]] =
   *     TypesafeConfig.fromTypesafeConfig(ConfigFactory.load.resolve, descriptor[MyConfig])
   * }}}
   */
  def fromTypesafeConfig[A](
    conf: => com.typesafe.config.Config,
    configDescriptor: ConfigDescriptor[A]
  )(implicit tag: Tag[A]): Layer[ReadError[String], A] =
    ZConfig.fromConfigDescriptor(
      configDescriptor from TypesafeConfigSource.fromTypesafeConfig(ZIO.succeed(conf))
    )
}
