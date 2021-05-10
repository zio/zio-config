package zio.config

import com.typesafe.config.{ConfigObject, ConfigRenderOptions}

package object typesafe {
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
}
