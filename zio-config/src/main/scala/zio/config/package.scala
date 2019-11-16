package zio

import zio.config.actions.{ ConfigDocs, Read, Write }

package object config {

  type MultiKeyReadErrors[K, V] = ReadErrors[Vector[K], V]
  type ConfigErrors[K, V]       = ReadErrors[Vector[K], V]

  def read[K, V, A](config: => ConfigDescriptor[K, V, A]): IO[ReadErrors[Vector[K], V], A] =
    Read.read(config)

  def write[K, V, A](config: => ConfigDescriptor[K, V, A], a: A): Either[String, PropertyTree[K, V]] =
    Write.write[K, V, A](config, a)

  def generateDocs[K, V, A](config: => ConfigDescriptor[K, V, A]): ConfigDocs[K, V] =
    ConfigDocs.createDoc[K, V, A](config)

  def generateDocsWithValue[K, V, A](config: => ConfigDescriptor[K, V, A], value: A): Either[String, ConfigDocs[K, V]] =
    ConfigDocs.createDocWithValues[K, V, A](config, value)

  def config[A]: ZIO[Config[A], Nothing, A] =
    ZIO.accessM(_.config.config)

}
