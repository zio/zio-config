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
   *  val appConfigLayer  =
   *     ConfigSource.fromMap(Map("age" -> "20")).toLayer >>> configLayer(int("age"))
   *
   *  val app: ZIO[Has[MyConfig] with zio.console.Console,java.io.IOException, Unit] =
   *    getConfig[Int].flatMap(age => putStrLn(s"My age is ${age}"))
   *
   *  app.provideSomeLayer[Console](appConfigLayer)
   *  // ZIO[zio.console.Console, Exception, Unit]
   * }}}
   */
  final def configLayer[A](config: ConfigDescriptor[A])(implicit
    tag: Tag[A]
  ): ZLayer[Has[ConfigSource], ReadError[K], Has[A]] =
    ZIO.accessM[Has[ConfigSource]](source => read(config from source.get)).toLayer

  /**
   * Example usage:
   *
   * {{{
   *  val config =
   *    int("age") from ConfigSource.fromMap(Map("age" -> "20")
   *
   *  val appConfigLayer  =
   *     configLayer_(config)
   *
   *  val app: ZIO[Has[MyConfig] with zio.console.Console,java.io.IOException, Unit] =
   *    getConfig[Int].flatMap(age => putStrLn(s"My age is ${age}"))
   *
   *  app.provideSomeLayer[Console](appConfigLayer)
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
   * {{{
   *   final case class MyConfig(username: String, port: Int)
   *
   *   val app: ZIO[Has[MyConfig] with zio.console.Console,java.io.IOException, Unit] =
   *     for {
   *        config <- getConfig[MyConfig]
   *        _      <- putStrLn(config.toString)
   *     } yield ()
   *
   *   val config =
   *     (string("USERNAME") |@| int("PORT")).to[MyConfig] from ConfigSource.fromMap(Map())
   *
   *   val main: ZIO[zio.console.Console, Exception, Unit] =
   *     app.provideSomeLayer[Console](configLayer_(config))
   *
   * }}}
   */
  final def getConfig[A](implicit tag: Tag[A]): ZIO[Has[A], Nothing, A] =
    ZIO.access(_.get)
}
