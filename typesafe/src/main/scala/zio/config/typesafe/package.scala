package zio.config

import com.typesafe.config.{ConfigObject, ConfigRenderOptions}

package object typesafe extends TreeToHoconSupport {
  implicit class PropertyTreeOps(tree: PropertyTree[String, String]) { self =>

    /**
     * Convert property tree to type-safe ConfigObject
     * @return
     */
    def toHocon: ConfigObject =
      propertyTreeToTypesafeConfig(tree)

    /**
     * A helper function that convert property tree to a HOCON string.
     * Other options are directly available in ConfigObject of typesafe.
     */
    def toHoconString: String =
      toHocon.render(ConfigRenderOptions.concise().setFormatted(true).setJson(false))

    /**
     * A helper function that convert property tree to a HOCON string
     * Other options are directly available in ConfigObject of typesafe.
     */
    def toJson: String =
      toHocon.render(ConfigRenderOptions.concise().setFormatted(true).setJson(true))
  }
}