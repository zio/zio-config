package zio.config

import zio.{ Has, Tag, ZIO }

trait ConfigModule extends ConfigDocsModule with ReadModule with WriteModule {
  type ZConfig[A] = Has[A]

  final def getConfig[A](implicit tag: Tag[A]): ZIO[ZConfig[A], Nothing, A] =
    ZIO.access(_.get)
}
