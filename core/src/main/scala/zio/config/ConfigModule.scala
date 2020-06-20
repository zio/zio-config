package zio.config

import zio.{ Has, Tag, ZIO }

trait ConfigModule extends ConfigDocsModule with ReadModule with WriteModule {
  type Config[A] = Has[A]

  final def config[A](implicit tag: Tag[A]): ZIO[Config[A], Nothing, A] =
    ZIO.access(_.get)
}
