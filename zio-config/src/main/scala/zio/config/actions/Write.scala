package zio.config.actions

import zio.ZIO
import zio.config.Config

final case class Write[A](run: ZIO[A, String, Map[String, String]])

object Write {
  final def write[A](config: Config[A]): Write[A] =
    config match {
      case Config.Pure(_) =>
        Write(ZIO.access(_ => Map.empty))

      case Config.Source(path, propertyType) =>
        Write(ZIO.access { aa =>
          Map(path -> propertyType.write(aa))
        })

      case Config.Optional(c) =>
        Write(
          ZIO.accessM(
            a =>
              a.asInstanceOf[Option[A]]
                .fold[ZIO[A, String, Map[String, String]]](
                  ZIO.succeed(Map.empty[String, String])
                )(aa => write(c).run.provide(aa))
          )
        )

      case Config.OnError(c, _) =>
        Write(
          ZIO.accessM(
            b =>
              write(c).run
                .provide(b)
                .fold(
                  _ => Map.empty,
                  success => success
                )
          )
        )

      case Config.MapEither(c, _, to) =>
        Write(ZIO.accessM { b =>
          to(b) match {
            case Right(before) =>
              write(c).run.provide(before)

            case Left(error) => ZIO.fail(error)
          }
        })

      case Config.Or(left, right) =>
        Write(ZIO.accessM(env => env.fold(a => write(left).run.provide(a), b => write(right).run.provide(b))))

      case Config.Zip(config1, config2) =>
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
