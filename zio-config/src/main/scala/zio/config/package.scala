package zio

import zio.config.ReadErrors.ReadError
import zio.config.actions.{ ConfigDocs, Read, Write }

package object config {
  def read[K, V, A](config: => ConfigDescriptor[K, V, A]): IO[ReadErrors[K, V], A] =
    Read.read(config)

  def write[K, V, A](config: => ConfigDescriptor[K, V, A], a: A): Either[String, PropertyTree[K, V]] =
    Write.write[K, V, A](config, a)

  def docs[K, V, A](config: => ConfigDescriptor[K, V, A], value: Option[A]): ConfigDocs[K, V] =
    ConfigDocs.createDoc[K, V, A](config, value)

  def config[A]: ZIO[Config[A], Nothing, A] =
    ZIO.accessM(_.config.config)

  def getConfigValue[K, V](path: Vector[K]): ZIO[ConfigSource[Vector[K], V], ReadError[Vector[K], V], V] =
    ZIO.accessM[ConfigSource[Vector[K], V]](_.getConfigValue(path))
}
