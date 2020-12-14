package zio.config

import zio.{ Has, Tag, ZIO }

trait ConfigModule extends ConfigDocsModule with ReadModule with WriteModule {
  final def getConfig[A](implicit tag: Tag[A]): ZIO[Has[A], Nothing, A] =
    ZIO.access(_.get)
}
