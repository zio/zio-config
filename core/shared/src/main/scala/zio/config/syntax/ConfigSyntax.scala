package zio.config.syntax

import zio.config.TupleConversion, TupleConversion._
import zio.Config
import zio.ConfigProvider
import zio.config.Read

// To be moved to ZIO ?
// Or may be zio-config can be considered as an extension to ZIO
trait ConfigSyntax {
  import zio.config.VersionSpecificSupport._

  implicit class ConfigOps[A](config: zio.Config[A]) { self =>

    def to[B <: Product](implicit conv: TupleConversion[B, A]): Config[B] =
      config.map(
        conv.from
      )

    // To reduce the number of changes in examples
    // Example: read(config from ConfigProvider.fromMap(""))
    def from(configProvider: ConfigProvider): Read[A] =
      Read(config, configProvider)

  }

  // To be moved to ZIO
  implicit class FromConfigTypesafe(c: Config.type) {
    def collectAll[A](head: => Config[A], tail: Config[A]*): Config[List[A]] =
      tail.reverse
        .map(Config.defer(_))
        .foldLeft[Config[(A, List[A])]](
          Config
            .defer(head)
            .map((a: A) => (a, Nil))
        )((b: Config[(A, List[A])], a: Config[A]) =>
          (b.zip[A](a)
            .map({ case (first, tail, a) =>
              (first, a :: tail)
            }))
        )
        .map { case (a, t) => a :: t }
  }

  // To be moved to ZIO
  implicit class FromConfigProvider(c: ConfigProvider.type) {
    def collectAll[A](head: => Config[A], tail: Config[A]*): Config[List[A]] =
      tail.reverse
        .map(Config.defer(_))
        .foldLeft[Config[(A, List[A])]](
          Config
            .defer(head)
            .map((a: A) => (a, Nil))
        )((b: Config[(A, List[A])], a: Config[A]) =>
          (b.zip[A](a)
            .map({ case (first, tail, a) =>
              (first, a :: tail)
            }))
        )
        .map { case (a, t) => a :: t }
  }
}
