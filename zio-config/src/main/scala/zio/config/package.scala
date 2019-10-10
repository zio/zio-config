package zio

import zio.config.actions.{ ConfigDocs, Read, Write }

package object config extends Sources {

  def read[A](config: => ConfigDescriptor[A]): ZIO[ConfigSource, ReadErrors, A] =
    Read.read[A](config)

  def write[A](config: => ConfigDescriptor[A]): ZIO[A, String, Map[String, String]] =
    Write.write[A](config)

  def docs[A](config: => ConfigDescriptor[A], value: Option[A]): ConfigDocs =
    ConfigDocs.createDoc[A](config, value)

  def config[A]: ZIO[Config[A], Nothing, A] =
    ZIO.accessM(_.config.config)

  def getConfigValue(path: String): ZIO[ConfigSource, ReadError, String] =
    ZIO.accessM(_.configService.getConfigValue(path))
}
