package zio

import zio.config.ReadErrors.ReadError
import zio.config.actions.{ ConfigDocs, Read, Write }

package object config {

  def read[A](config: => ConfigDescriptor[A]): ZIO[ConfigSource[String, String], ReadErrors[String, String], A] =
    Read.read[A](config)

  def write[A](config: => ConfigDescriptor[A], a: A): Either[String, PropertyTree[String, String]] =
    Write.write[A](config, a)

  def docs[A](config: => ConfigDescriptor[A], value: Option[A]): ConfigDocs =
    ConfigDocs.createDoc[A](config, value)

  def config[A]: ZIO[Config[A], Nothing, A] =
    ZIO.accessM(_.config.config)

  def getConfigValue[K, V](path: List[K]): ZIO[ConfigSource[K, V], ReadError[K, V], V] =
    ZIO.accessM(_.configSourceService.getConfigValue(path))
}
