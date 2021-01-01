package zio.config.typesafe

import com.typesafe.config.ConfigFactory
import zio.{Has, Layer, Tag}
import zio.config._

import java.io.File

object TypesafeConfig extends BaseTypesafeConfig {

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
  def fromHoconFile[A](file: File, configDescriptor: ConfigDescriptor[A])(
    implicit tag: Tag[A]
  ): Layer[ReadError[String], Has[A]] =
    fromTypesafeConfig(ConfigFactory.parseFile(file).resolve, configDescriptor)
}
