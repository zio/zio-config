package zio.config.actions

import zio.config.{ Config, KeyValue }
import zio.ZIO

final case class Write[A](run: ZIO[A, Nothing, KeyValue])

object Write {

  final def write[A](config: Config[A]): Write[A] =
    config match {
      case Config.Source(path, propertyType) =>
        Write(ZIO.access(aa => {
          KeyValue(Map(path -> propertyType.write(aa)))
        }))

      case Config.Sources(propertyType, paths) =>
        Write(
          ZIO.access(
            aa => {
              val map =
                paths.foldLeft(Map.empty[String, String]) {
                (m, path) =>
                  val str = propertyType.write(aa)
                  Map(path -> str) ++ m
              }
              KeyValue(
                map
              )
            }
          )
        )

      case Config.Xmap(c, _, to) =>
        Write(ZIO.accessM(b => write(c).run.provide(to(b))))

      case Config.OnError(c, _, _) => Write(write(c).run)

        // TODO mapping function is not used here
      case Config.Map(c, _) => Write(write(c).run)

      case Config.ErrorMap(c, _) => Write(write(c).run)

      case Config.OrElseEither(left, right) =>
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
