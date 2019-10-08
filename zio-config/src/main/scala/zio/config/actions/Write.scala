package zio.config.actions

import zio.ZIO
import zio.config.ConfigDescriptor

object Write {
  final def write[A](config: ConfigDescriptor[A]): ZIO[A, String, Map[String, String]] =
    config match {
      case ConfigDescriptor.Empty() =>
        ZIO.access(_ => Map.empty)

      case ConfigDescriptor.Source(path, propertyType) =>
        ZIO.access { aa =>
          Map(path -> propertyType.write(aa))
        }

      case ConfigDescriptor.Describe(c, _) =>
        write(c)

      case ConfigDescriptor.Optional(c) =>
        ZIO.accessM(
          _.fold[ZIO[A, String, Map[String, String]]](
            ZIO.succeed(Map.empty[String, String])
          )(aa => write(c).provide(aa))
        )

      case ConfigDescriptor.Default(c, _) =>
        write(c)

      case ConfigDescriptor.XmapEither(c, _, to) =>
        ZIO.accessM { b =>
          to(b) match {
            case Right(before) =>
              write(c).provide(before)

            case Left(error) =>
              ZIO.fail(error)
          }
        }

      case ConfigDescriptor.OrElseEither(left, right) =>
        ZIO.accessM(env => env.fold(a => write(left).provide(a), b => write(right).provide(b)))

      case ConfigDescriptor.Zip(config1, config2) =>
        ZIO.accessM(
          env =>
            write(config1)
              .provide(env._1)
              .flatMap(value => write(config2).provide(env._2).map(kv => value ++ kv))
        )
    }
}
