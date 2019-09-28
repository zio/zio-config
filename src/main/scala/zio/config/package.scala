package zio

import java.net.URI

import zio.config.actions.{ Read, Report, Write }

package object config extends ConfigSyntax with Sources {
  def int(path: String): Config[Int]       = Config.Source(path, PropertyType.IntType)
  def double(path: String): Config[Double] = Config.Source(path, PropertyType.DoubleType)
  def string(path: String): Config[String] = Config.Source(path, PropertyType.StringType)
  def long(path: String): Config[Long]     = Config.Source(path, PropertyType.LongType)
  def short(path: String): Config[Short]   = Config.Source(path, PropertyType.ShortType)
  def uri(path: String): Config[URI]       = Config.Source(path, PropertyType.UriType)

  type ConfigOption[A] = Config[Option[A]]

  def opt[A](config: => Config[A]): Config[Option[A]] =
    config
      .mapEither[Option[A]](a => Right(Some(a)))({
        case Some(value) => Right(value)
        case None        => Left(WriteError("Error: Cannot write a none value", None))
      })
      .onError(_ => None)

  def read[A](config: => Config[A]): Read[A]     = Read.read[A](config)
  def write[A](config: => Config[A]): Write[A]   = Write.write[A](config)
  def report[A](config: => Config[A]): Report[A] = Report.report[A](config)
}
