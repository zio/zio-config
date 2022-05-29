package zio.config

import com.typesafe.config.{ConfigObject, ConfigRenderOptions, ConfigValue}
import zio._

import java.io.File

package object typesafe {
  implicit class FromConfigTypesafe(c: ZConfig.type) {
    def fromResourcePath[A](configDescriptor: ConfigDescriptor[A])(implicit
      tag: Tag[A]
    ): Layer[ReadError[String], A] =
      TypesafeConfig.fromResourcePath(configDescriptor)

    def fromHoconFile[A](file: File, configDescriptor: ConfigDescriptor[A])(implicit
      tag: Tag[A]
    ): Layer[ReadError[String], A] =
      TypesafeConfig.fromHoconFile(file, configDescriptor)

    def fromHoconFilePath[A](filePath: String, configDescriptor: ConfigDescriptor[A])(implicit
      tag: Tag[A]
    ): Layer[ReadError[String], A] =
      TypesafeConfig.fromHoconFilePath(filePath, configDescriptor)

    def fromHoconString[A](hoconString: String, configDescriptor: ConfigDescriptor[A])(implicit
      tag: Tag[A]
    ): Layer[ReadError[String], A] =
      TypesafeConfig.fromHoconString(hoconString, configDescriptor)

    def fromTypesafeConfig[A](
      conf: ZIO[Any, Throwable, com.typesafe.config.Config],
      configDescriptor: ConfigDescriptor[A]
    )(implicit tag: Tag[A]): Layer[ReadError[String], A] =
      TypesafeConfig.fromTypesafeConfig(conf, configDescriptor)
  }

  implicit class FromConfigSourceTypesafe(c: ConfigSource.type) {
    def fromResourcePath: ConfigSource =
      TypesafeConfigSource.fromResourcePath

    def fromHoconFile[A](file: File): ConfigSource =
      TypesafeConfigSource.fromHoconFile(file)

    def fromHoconFilePath[A](filePath: String): ConfigSource =
      TypesafeConfigSource.fromHoconFilePath(filePath)

    def fromHoconString(input: String): ConfigSource =
      TypesafeConfigSource.fromHoconString(input)

    def fromTypesafeConfig(
      rawConfig: ZIO[Any, Throwable, com.typesafe.config.Config]
    ): ConfigSource =
      TypesafeConfigSource.fromTypesafeConfig(rawConfig)
  }

  implicit class PropertyTreeOps(tree: PropertyTree[String, String]) { self =>

    /**
     * Convert property tree to type-safe ConfigObject
     */
    def toHocon: ConfigObject =
      TypesafeConfigSource.fromPropertyTree(tree)

    /**
     * A helper function that convert property tree to a HOCON string.
     * Other options are directly available in ConfigObject of typesafe.
     */
    def toHoconString: String =
      toHocon.render(
        ConfigRenderOptions.concise().setFormatted(true).setJson(false)
      )

    /**
     * A helper function that convert property tree to a Json string
     * Other options are directly available in ConfigObject of typesafe.
     */
    def toJson: String =
      toHocon.render(
        ConfigRenderOptions.concise().setFormatted(true).setJson(true)
      )
  }

  implicit class ConfigDescriptorOps[A](a: => A) { self =>

    /**
     * Convert your config value `A` to `com.typesafe.config.ConfigObject`, given its
     * `ConfigDescriptor`
     */
    def toHocon(
      configDescriptor: ConfigDescriptor[A]
    ): Either[String, ConfigObject] =
      write(configDescriptor, a).map(_.toHocon)

    /**
     * Convert your config value `A` to a verbose Hocon string, given its
     * `ConfigDescriptor`
     */
    def toHoconString(
      configDescriptor: ConfigDescriptor[A]
    ): Either[String, String] =
      write(configDescriptor, a).map(_.toHoconString)

    /**
     * Convert your config value `A` to a concise Json string, given its
     * `ConfigDescriptor`
     */
    def toJson(configDescriptor: ConfigDescriptor[A]): Either[String, String] =
      write(configDescriptor, a).map(_.toJson)

  }

  val configValueConfigDescriptor: ConfigDescriptor[ConfigValue] =
    ConfigDescriptorAdt.sourceDesc(ConfigSource.empty, ConfigValuePropertyType)

}
