package zio.config.typesafe

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
   *   import zio.config.magnolia.descriptor
   *
   *   case class MyConfig(port: Int, url: String)
   *
   *   val result: Layer[Config.Error, Has[MyConfig]] =
   *     TypesafeConfig.fromResourcePath(descriptor[MyConfig])
   * }}}
   */
  def fromResourcePath[A](
    configDescriptor: Config[A]
  )(implicit tag: Tag[A]): Layer[Config.Error, A] =
    ZConfig.fromConfig(
      configDescriptor from TypesafeConfigSource.fromResourcePath
    )

  /**
   * Retrieve your config from a HOCON file
   *
   * A complete example usage:
   *
   * {{{
   *
   *   import zio.config.magnolia.descriptor
   *
   *   case class MyConfig(port: Int, url: String)
   *
   *   val result: Layer[Config.Error, Has[MyConfig]] =
   *     TypesafeConfig.fromHoconFile(new File("/path/to/xyz.hocon"), descriptor[MyConfig])
   * }}}
   */
  def fromHoconFile[A](file: File, configDescriptor: Config[A])(implicit
    tag: Tag[A]
  ): Layer[Config.Error, A] =
    ZConfig.fromConfig(
      configDescriptor from TypesafeConfigSource.fromHoconFile(file)
    )

  /**
   * Retrieve your config from a path to HOCON file
   *
   * A complete example usage:
   *
   * {{{
   *
   *   import zio.config.magnolia.descriptor
   *
   *   case class MyConfig(port: Int, url: String)
   *
   *   val result: Layer[Config.Error, Has[MyConfig]] =
   *     TypesafeConfig.fromHoconFilePath("/path/to/xyz.hocon", descriptor[MyConfig])
   * }}}
   */
  def fromHoconFilePath[A](filePath: String, configDescriptor: Config[A])(implicit
    tag: Tag[A]
  ): Layer[Config.Error, A] =
    ZConfig.fromConfig(
      configDescriptor from TypesafeConfigSource.fromHoconFilePath(filePath)
    )

  /**
   * Retrieve a config from a given Hocon string.
   *
   * A complete example usage:
   *
   * {{{
   *
   *   import zio.config.magnolia.descriptor
   *
   *   case class MyConfig(port: Int, url: String)
   *
   *   val configString = """port: 10, url: "http://x.y""""
   *
   *   val result: Layer[Config.Error, Has[MyConfig]] =
   *     TypesafeConfig.fromHoconString(configString, descriptor[MyConfig])
   * }}}
   */
  def fromHoconString[A](hoconString: String, configDescriptor: Config[A])(implicit
    tag: Tag[A]
  ): Layer[Config.Error, A] =
    ZConfig.fromConfig(
      configDescriptor from TypesafeConfigSource.fromHoconString(hoconString)
    )

  /**
   * Retrieve a config from `com.typesafe.config.Config`
   *
   * A complete example usage:
   *
   * {{{
   *
   *   import zio.config.magnolia.descriptor
   *
   *   case class MyConfig(port: Int, url: String)
   *
   *   val result: Layer[Config.Error, Has[MyConfig]] =
   *     TypesafeConfig.fromTypesafeConfig(ZIO.attempt(ConfigFactory.load.resolve), descriptor[MyConfig])
   * }}}
   */
  def fromTypesafeConfig[A](
    conf: ZIO[Any, Throwable, com.typesafe.config.Config],
    configDescriptor: Config[A]
  )(implicit tag: Tag[A]): Layer[Config.Error, A] =
    ZConfig.fromConfig(
      configDescriptor from TypesafeConfigSource.fromTypesafeConfig(conf)
    )
}
