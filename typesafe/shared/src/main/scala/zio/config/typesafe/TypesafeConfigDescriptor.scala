package zio.config.typesafe

import com.typesafe.config._
import zio.config._

object TypesafeConfigDescriptor {
  val configValueConfigDescriptor: ConfigDescriptor[ConfigValue] =
    ConfigDescriptorAdt.sourceDesc(ConfigSource.empty, TypesafePropertyType.ConfigValuePropertyType)
}
