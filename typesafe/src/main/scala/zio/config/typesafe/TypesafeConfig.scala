package zio.config.typesafe

import com.typesafe.config.ConfigFactory
import zio.config._
import zio.{Has, Layer, Tag, ZIO, ZLayer}

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
  def fromHoconFile[A](file: File, configDescriptor: ConfigDescriptor[A])(implicit
    tag: Tag[A]
  ): Layer[ReadError[String], Has[A]] =
    fromTypesafeConfig(ConfigFactory.parseFile(file).resolve, configDescriptor)

  /**
   * Retrieve a config from a Hocon file calculated by an effect
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
   *     TypesafeConfig.fromHoconFileM(ZIO.succeed(new File("/path/to/xyz.hocon")), descriptor[MyConfig])
   * }}}
   */
  def fromHoconFileM[R, E >: ReadError[String], A](getFile: ZIO[R, E, File], configDescriptor: ConfigDescriptor[A])(
    implicit tag: Tag[A]
  ): ZLayer[R, E, Has[A]] =
    fromTypesafeConfigM(
      getFile.flatMap { file =>
        ZIO
          .effect(ConfigFactory.parseFile(file).resolve)
          .mapError(failure => ReadError.SourceError(message = failure.getMessage))
      },
      configDescriptor
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
   *   val result: Layer[ReadError[String], Has[MyConfig]] =
   *     TypesafeConfig.fromHoconString(configString, descriptor[MyConfig])
   * }}}
   */
  def fromHoconString[A](str: String, configDescriptor: ConfigDescriptor[A])(implicit
    tag: Tag[A]
  ): Layer[ReadError[String], Has[A]] =
    fromTypesafeConfig(ConfigFactory.parseString(str).resolve, configDescriptor)

  /**
   * Retrieve a config from a HOCON string value produced by an effect
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
   *   val result: Layer[ReadError[String], Has[MyConfig]] =
   *     TypesafeConfig.fromHoconStringM(ZIO.succeed(configString), descriptor[MyConfig])
   * }}}
   */
  def fromHoconStringM[R, E >: ReadError[String], A](getStr: ZIO[R, E, String], configDescriptor: ConfigDescriptor[A])(
    implicit tag: Tag[A]
  ): ZLayer[R, E, Has[A]] =
    fromTypesafeConfigM(
      getStr.flatMap { str =>
        ZIO
          .effect(ConfigFactory.parseString(str).resolve)
          .mapError(failure => ReadError.SourceError(message = failure.getMessage))
      },
      configDescriptor
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
  )(implicit tag: Tag[A]): Layer[ReadError[String], Has[A]] =
    ZConfig.fromConfigDescriptorM(
      ZIO
        .fromEither(TypesafeConfigSource.fromTypesafeConfig(conf))
        .map(configDescriptor from _)
    )

  /**
   * Retrieve a config from `com.typesafe.config.Config` returned by an effect
   *
   * A complete example usage:
   *
   * {{{
   *
   *   import zio.config.magnolia.DeriveConfigDescriptor.descriptor
   *
   *   case class MyConfig(port: Int, url: String)
   *
   *   val result: Layer[Throwable, Has[MyConfig]] =
   *     TypesafeConfig.fromTypesafeConfigM(ZIO.effect(ConfigFactory.load.resolve), descriptor[MyConfig])
   * }}}
   */
  def fromTypesafeConfigM[R, E >: ReadError[String], A](
    getConfig: ZIO[R, E, com.typesafe.config.Config],
    configDescriptor: ConfigDescriptor[A]
  )(implicit tag: Tag[A]): ZLayer[R, E, Has[A]] =
    ZConfig.fromConfigDescriptorM(
      for {
        config    <- getConfig
        rawConfig <- ZIO.fromEither(TypesafeConfigSource.fromTypesafeConfig(config))
      } yield configDescriptor from rawConfig
    )
}
