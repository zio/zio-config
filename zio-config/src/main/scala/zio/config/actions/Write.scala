package zio.config.actions

import zio.ZIO
import zio.config.ConfigDescriptor

final case class Write[A](run: ZIO[A, String, Map[String, String]])

object Write {
  final def write[A](config: ConfigDescriptor[A]): Write[A] =
    config match {
      case ConfigDescriptor.Succeed(_) =>
        Write(ZIO.access(_ => Map.empty))

      case ConfigDescriptor.Source(path, propertyType) =>
        Write(ZIO.access { aa =>
          Map(path -> propertyType.write(aa))
        })

      case ConfigDescriptor.Describe(c, _) =>
        write(c)

      case ConfigDescriptor.Optional(c) =>
        Write(
          ZIO.accessM(
            _.fold[ZIO[A, String, Map[String, String]]](
              ZIO.succeed(Map.empty[String, String])
            )(aa => write(c).run.provide(aa))
          )
        )

      // No need to write the default values back into the env
      case ConfigDescriptor.Default(c, _) =>
        write(c)

      case ConfigDescriptor.XmapEither(c, _, to) =>
        Write(ZIO.accessM { b =>
          to(b) match {
            case Right(before) =>
              write(c).run.provide(before)

            case Left(error) => ZIO.fail(error)
          }
        })

      case ConfigDescriptor.OrElseEither(left, right) =>
        Write(ZIO.accessM(env => env.fold(a => write(left).run.provide(a), b => write(right).run.provide(b))))

      case ConfigDescriptor.Zip(config1, config2) =>
        Write(
          ZIO.accessM(
            env =>
              write(config1).run
                .provide(env._1)
                .flatMap(value => write(config2).run.provide(env._2).map(kv => value ++ kv))
          )
        )
    }
}
