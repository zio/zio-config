package zio.config.syntax

import zio.config.TupleConversion, TupleConversion._
import zio.Config

// To be moved to ZIO ?
// Or may be zio-config can be considered as an extension to ZIO
trait ConfigSyntax {
  import zio.config.VersionSpecificSupport._

  implicit class ConfigOps[A](config: zio.Config[A]) { self =>

    def to[B <: Product](implicit conv: TupleConversion[B, A]): Config[B] =
      config.map(
        conv.from
      )
  }
}
