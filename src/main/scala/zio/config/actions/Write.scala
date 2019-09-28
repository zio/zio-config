package zio.config.actions

import zio.config.{ Config, KeyValue, WriteError }
import zio.ZIO

final case class Write[A](run: ZIO[A, WriteError, KeyValue])

object Write {

  // Write
  final def write[A](config: Config[A]): Write[A] =
    config match {
      case Config.Source(path, propertyType) =>
        Write(ZIO.access(aa => {
          KeyValue(Map(path -> propertyType.write(aa)))
        }))

      case Config.Xmap(c, _, to) =>
        Write(ZIO.accessM(b => write(c).run.provide(to(b))))

      case Config.OnError(c, _) =>
        Write(
          ZIO.accessM(
            b =>
              write(c).run
                .provide(b)
                .fold(
                  _ => KeyValue(Map.empty),
                  success => success
                )
          )
        )

      case Config.ErrorXMap(c, _, to) =>
        Write(ZIO.accessM(b => {
          to(b) match {
            case Right(before) =>
              write(c).run.provide(before)

            case Left(error) => ZIO.fail(error)
          }
        }))

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
