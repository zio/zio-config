package zio.config

import zio.{IsNotIntersection, Tag, ZIO, ZLayer}

trait ConfigModule
    extends ConfigDescriptorModule
    with ConfigSourceModule
    with ConfigDocsModule
    with ReadModule
    with WriteModule {

  /**
   * Example usage:
   *
   * {{{
   *
   *  val appConfigLayer  =
   *     ConfigSource.fromMap(Map("age" -> "20")).toLayer >>> configLayer(int("age"))
   *
   *  val app: ZIO[MyConfig with zio.console.Console,java.io.IOException, Unit] =
   *    getConfig[Int].flatMap(age => putStrLn(s"My age is " + age.toString))
   *
   *  app.provideSomeLayer[Console](appConfigLayer)
   *  // ZIO[zio.console.Console, Exception, Unit]
   *
   * }}}
   *
   * This can also be simplified to
   *
   * {{{
   *   val appConfigLayer  =
   *    configLayer_(int("age") from  ConfigSource.fromMap(Map("age" -> "20")))
   *
   *   app.provideSomeLayer[Console](appConfigLayer)
   *
   * }}}
   *
   * The preference depends on how you want to design the entry point to managing config
   * of your app.
   */
  final def configLayer[A: Tag: IsNotIntersection](config: ConfigDescriptor[A]): ZLayer[ConfigSource, ReadError[K], A] =
    ZIO.service[ConfigSource].flatMap(source => read(config from source)).toLayer

  /**
   * Example usage:
   *
   * {{{
   *
   *  final case class MyConfig(age: Int)
   *
   *  val configDesc =
   *    int("age").to[MyConfig] from ConfigSource.fromMap(Map("age" -> "20"))
   *
   *  val app: ZIO[Has[MyConfig] with zio.console.Console,java.io.IOException, Unit] =
   *    getConfig[MyConfig].flatMap(config => putStrLn("My age is " + config.age.toString))
   *
   *  app.provideSomeLayer[Console](configLayer_(configDesc))
   *  // ZIO[zio.console.Console, Exception, Unit]
   * }}}
   */
  final def configLayer_[A: Tag: IsNotIntersection](config: ConfigDescriptor[A]): ZLayer[Any, ReadError[K], A] =
    ConfigSource.empty.toLayer >>> configLayer(config)

  /**
   * Example usage:
   *
   * A helper method allowing you to forget passing configs as arguments to all over the place
   * in your app. Whereever, you need to access the config, simply
   * call `getConfig[MyConfig].flatMap(config => ???)`.
   *
   * PS: if you are familiar with Kleisli, this is similar
   * to using `Kleisi[IO, Config, A]`, except for the fact that it is `Has[Config]`
   * instead of `Config` allowing you to mixin with other dependencies keeping your `Monad`
   * the same
   *
   * {{{
   *   import zio.console._
   *
   *   final case class MyConfig(username: String, port: Int)
   *
   *   val app: ZIO[Has[MyConfig] with Console, java.io.IOException, Unit] =
   *     for {
   *        config <- getConfig[MyConfig]
   *        _      <- putStrLn(config.toString)
   *     } yield ()
   *
   *   val configDesc =
   *     (string("USERNAME") |@| int("PORT")).to[MyConfig] from ConfigSource.fromMap(Map())
   *
   *   val main: ZIO[zio.console.Console, Exception, Unit] =
   *     app.provideSomeLayer[Console](configLayer_(configDesc))
   *
   * }}}
   */
  final def getConfig[A: Tag: IsNotIntersection]: ZIO[A, Nothing, A] =
    ZIO.service[A]
}
