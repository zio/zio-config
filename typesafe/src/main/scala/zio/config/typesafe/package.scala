package zio.config

import com.typesafe.config.ConfigObject

package object typesafe extends TreeToHoconSupport {
  implicit class PropertyTreeOps(tree: PropertyTree[String, String]) { self =>
    def toHocon: ConfigObject =
      treeToTypesafeConfig(tree)
  }
}
