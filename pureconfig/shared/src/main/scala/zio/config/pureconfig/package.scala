package zio.config

import _root_.pureconfig.ConfigReader
import zio.Config
import com.typesafe.config.ConfigFactory
import scala.util.Try

package object pureconfig {
  def fromPureconfig[A](implicit cc: ConfigReader[A]): Config[A] =
    Config.string.mapOrFail(propertyValue => Try(ConfigFactory.parseString(propertyValue).resolve)
      .map(_.root)
      .orElse(Try(ConfigFactory.parseString(s"k: $propertyValue").resolve).map(_.root.get("k")))
      .toEither
      .left.map(throwable => Config.Error.InvalidData(message = throwable.getMessage))
      .flatMap(cc.from(_).left.map(failures => Config.Error.InvalidData(message = failures.toString()))))

}
