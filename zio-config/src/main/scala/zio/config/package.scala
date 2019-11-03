package zio

import zio.config.ReadErrors.ReadError
import zio.config.actions.{ ConfigDocs, Read, Write }

package object config {

  def read[K, V, A](config: => ConfigDescriptor[Vector[K], V, A]): ZIO[ConfigSource[Vector[K], V], ReadErrors[Vector[K], V], A] =
    Read.read[K, V, A](config)

  def write[K, V, A](config: => ConfigDescriptor[Vector[K], V, A], a: A): Either[String, PropertyTree[String, String]] =
    Write.write[A](config, a)

  def docs[A](config: => ConfigDescriptor[K, V, A], value: Option[A]): ConfigDocs =
    ConfigDocs.createDoc[A](config, value)

  def config[A]: ZIO[Config[A], Nothing, A] =
    ZIO.accessM(_.config.config)

  def getConfigValue[K, V](path: Vector[K]): ZIO[ConfigSource[K, V], ReadError[K, V], V] =
    ZIO.accessM(_.configSourceService.getConfigValue(path))
}
