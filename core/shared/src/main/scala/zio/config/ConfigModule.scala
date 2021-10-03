package zio.config

import zio.{Has, Tag, ZIO, ZLayer}

trait ConfigModule extends ConfigDocsModule with ReadModule with WriteModule {
  final def getConfig[A](implicit tag: Tag[A]): ZIO[Has[A], Nothing, A] =
    ZIO.access(_.get)

   def configLayer[A](configDesc: ConfigDescriptor[A]): ZLayer[ConfigSource_, ReadError[K], Has[A]] = ???
   def loadConfig[A](configSource: ConfigSource_, configDesc: ConfigDescriptor[A]): ZLayer[Any, ReadError[K], Has[A]] = ???

}
