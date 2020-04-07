package zio.config

import com.typesafe.config.ConfigObject

package object typesafe extends PropertyTreeFunctions {
  implicit class PropertyTreeOps(tree: PropertyTree[String, String]) { self =>
    def toHocon: ConfigObject =
      treeToTypesafeConfig(tree)
  }
}
