package zio.config.typesafe

import com.github.ghik.silencer.silent
import com.typesafe.config._
import zio.config._

import scala.util.Try

@silent("Unused import")
object TypesafePropertyType {
  import PropertyType.PropertyReadError
  import VersionSpecificSupport._

  object ConfigValuePropertyType extends PropertyType[String, ConfigValue] {
    override def read(propertyValue: String): Either[PropertyReadError[String], ConfigValue] =
      Try(ConfigFactory.parseString(propertyValue).resolve)
        .map(_.root)
        .orElse(Try(ConfigFactory.parseString(s"k: $propertyValue").resolve).map(_.root.get("k")))
        .toEither
        .left
        .map(_ => PropertyReadError(propertyValue, "config-value"))

    override def write(a: ConfigValue): String = a.render(
      ConfigRenderOptions.concise().setFormatted(true).setJson(false)
    )
  }
}
