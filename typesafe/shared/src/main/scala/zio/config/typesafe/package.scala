package zio.config

import com.typesafe.config.{ConfigObject, ConfigRenderOptions, ConfigValue}
import zio._

import java.io.File

package object typesafe {

  implicit class FromConfigTypesafe(c: ZConfig.type) {
    def fromResourcePath[A](configDescriptor: Config[A])(implicit
      tag: Tag[A]
    ): Layer[Config.Error, A] =
      TypesafeConfig.fromResourcePath(configDescriptor)

    def fromHoconFile[A](file: File, configDescriptor: Config[A])(implicit
      tag: Tag[A]
    ): Layer[Config.Error, A] =
      TypesafeConfig.fromHoconFile(file, configDescriptor)

    def fromHoconFilePath[A](filePath: String, configDescriptor: Config[A])(implicit
      tag: Tag[A]
    ): Layer[Config.Error, A] =
      TypesafeConfig.fromHoconFilePath(filePath, configDescriptor)

    def fromHoconString[A](hoconString: String, configDescriptor: Config[A])(implicit
      tag: Tag[A]
    ): Layer[Config.Error, A] =
      TypesafeConfig.fromHoconString(hoconString, configDescriptor)

    def fromTypesafeConfig[A](
      conf: ZIO[Any, Throwable, com.typesafe.config.Config],
      configDescriptor: Config[A]
    )(implicit tag: Tag[A]): Layer[Config.Error, A] =
      TypesafeConfig.fromTypesafeConfig(conf, configDescriptor)
  }

  implicit class FromConfigProviderTypesafe(c: ConfigProvider.type) {
    def fromResourcePath: ConfigProvider =
      TypesafeConfigSource.fromResourcePath_

    def fromHoconString(input: String): ConfigProvider =
      TypesafeConfigSource.fromHoconString_(input)

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

  implicit class ConfigOps[A](a: => A) { self =>

    /**
     * Convert your config value `A` to `com.typesafe.config.ConfigObject`, given its
     * `Config`
     */
    def toHocon(
      configDescriptor: Config[A]
    ): Either[String, ConfigObject] =
      write(configDescriptor, a).map(_.toHocon)

    /**
     * Convert your config value `A` to a verbose Hocon string, given its
     * `Config`
     */
    def toHoconString(
      configDescriptor: Config[A]
    ): Either[String, String] =
      write(configDescriptor, a).map(_.toHoconString)

    /**
     * Convert your config value `A` to a concise Json string, given its
     * `Config`
     */
    def toJson(configDescriptor: Config[A]): Either[String, String] =
      write(configDescriptor, a).map(_.toJson)

  }

  val configValueConfig: Config[ConfigValue] =
    ConfigAdt.sourceDesc(ConfigSource.empty, ConfigValuePropertyType)

}
