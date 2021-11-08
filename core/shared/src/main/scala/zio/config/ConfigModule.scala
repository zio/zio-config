package zio.config

import zio.{Has, Tag, ZIO, ZLayer}

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
   *  val app: ZIO[Has[MyConfig] with zio.console.Console,java.io.IOException, Unit] =
   *    getConfig[Int].flatMap(age => putStrLn(s"My age is ${age}"))
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
  final def configLayer[A](config: ConfigDescriptor[A])(implicit
    tag: Tag[A]
  ): ZLayer[Has[ConfigSource], ReadError[K], Has[A]] =
    ZIO.accessM[Has[ConfigSource]](source => read(config from source.get)).toLayer

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
   *    getConfig[MyConfig].flatMap(config => putStrLn(s"My age is ${config.age}"))
   *
   *  app.provideSomeLayer[Console](configLayer_(configDesc))
   *  // ZIO[zio.console.Console, Exception, Unit]
   * }}}
   */
  final def configLayer_[A](config: ConfigDescriptor[A])(implicit
    tag: Tag[A]
  ): ZLayer[Any, ReadError[K], Has[A]] =
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
  final def getConfig[A](implicit tag: Tag[A]): ZIO[Has[A], Nothing, A] =
    ZIO.access(_.get)
}
