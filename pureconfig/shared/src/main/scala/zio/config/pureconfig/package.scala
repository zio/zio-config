package zio.config

import _root_.pureconfig.ConfigReader
import com.typesafe.config.ConfigFactory
import zio.Config

import scala.util.Try

package object pureconfig {

  /**
   * Turns a pureconfig `ConfigReader[A]` into a `Config[A]` that reads one string value at the current path. The text
   * is parsed as HOCON, first as a whole document (an object such as `host = localhost, port = 80`) and, when that
   * fails, as a single value (such as `8080` or `[a, b]`), and the result goes to the reader. It reads one value, not a
   * group of keys: for a case class, the whole object has to be in that one string.
   *
   * A parse error or a reader failure becomes a `Config.Error.InvalidData`. Because the text is HOCON, a string such as
   * `http://example.com` or `a=b` fails unless it is wrapped in double quotes.
   *
   * @see
   *   [[https://zio.dev/zio-config/integrations/ Integrations]]
   */
  def fromPureconfig[A](implicit cc: ConfigReader[A]): Config[A] =
    Config.string.mapOrFail(propertyValue =>
      Try(ConfigFactory.parseString(propertyValue).resolve)
        .map(_.root)
        .orElse(Try(ConfigFactory.parseString(s"k: $propertyValue").resolve).map(_.root.get("k")))
        .toEither
        .left
        .map(throwable => Config.Error.InvalidData(message = throwable.getMessage))
        .flatMap(cc.from(_).left.map(failures => Config.Error.InvalidData(message = failures.toString())))
    )

}
