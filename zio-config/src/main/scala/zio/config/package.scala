package zio

import zio.config.actions.{ ConfigDocs, Read, Write }

package object config {

  def read[A](config: => ConfigDescriptor[A]): ZIO[ConfigSource[String, String], ReadErrors[String, String], A] =
    Read.read[A](config)

  def write[A](config: => ConfigDescriptor[A]): ZIO[A, String, Map[String, String]] =
    Write.write[A](config)

  def docs[A](config: => ConfigDescriptor[A], value: Option[A]): ConfigDocs =
    ConfigDocs.createDoc[A](config, value)

  def config[A]: ZIO[Config[A], Nothing, A] =
    ZIO.accessM(_.config.config)

  def getConfigValue(
    path: String
  ): ZIO[ConfigSource[String, String], ReadError[String, String], PropertyTree[String, String]] =
    ZIO.accessM(_.configSourceService.getConfigValue(path))

  type ReadErrors[K, V] = ::[ReadError[K, V]]

  object ReadErrors {
    def apply[K, V](a: ReadError[K, V], as: ReadError[K, V]*): ReadErrors[K, V] =
      ::(a, as.toList)

    def concat[K, V](l: ReadErrors[K, V], r: ReadErrors[K, V]): ReadErrors[K, V] =
      ::(l.head, l.tail ++ r)

    def asThrowable[K, V, A](readErrors: ReadErrors[K, V]): RIO[system.System, A] =
      zio.system.lineSeparator.flatMap(t => ZIO.fail(new RuntimeException(readErrors.mkString(t))))
  }
}
