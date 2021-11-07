package zio.config

import zio.{Has, Tag, ZIO, ZLayer}

trait ConfigModule
    extends ConfigDescriptorModule
    with ConfigSourceModule
    with ConfigDocsModule
    with ReadModule
    with WriteModule {

  final def configLayer[A](config: ConfigDescriptor[A])(implicit
    tag: Tag[A]
  ): ZLayer[Has[ConfigSource], ReadError[K], Has[A]] =
    ZIO.accessM[Has[ConfigSource]](source => read(config from source.get)).toLayer

  final def configLayer_[A](config: ConfigDescriptor[A])(implicit
    tag: Tag[A]
  ): ZLayer[Any, ReadError[K], Has[A]] =
    ConfigSource.empty.toLayer >>> configLayer(config)

  final def getConfig[A](implicit tag: Tag[A]): ZIO[Has[A], Nothing, A] =
    ZIO.access(_.get)
}
