package zio

import zio.config.actions.{ ConfigDocs, Read, Write }

package object config extends Sources {

  def read[A](config: => ConfigDescriptor[A]): ZIO[ConfigSource, ReadErrors, (ConfigReport, A)] =
    Read.read[A](config)

  def write[A](config: => ConfigDescriptor[A]): Write[A] =
    Write.write[A](config)

  def docs[A](config: => ConfigDescriptor[A], value: Option[A]): ConfigDocs =
    ConfigDocs.docs[A](config, value)

  def config[A]: ZIO[Config[A], Nothing, A] = ZIO.accessM(_.config.config)

  def getConfigValue(path: String): ZIO[ConfigSource, ReadError, String] =
    ZIO.accessM(_.configService.getConfigValue(path))

  type ReadErrors = ::[ReadError]

  object ReadErrors {
    def apply(a: ReadError, as: ReadError*): ReadErrors =
      ::(a, as.toList)

    def concat(l: ReadErrors, r: ReadErrors): ReadErrors =
      ::(l.head, l.tail ++ r)
  }
}
