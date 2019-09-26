package zio

import java.net.URI

import zio.config.actions.{ Read, Report, Write }

package object config extends ConfigSyntax with Sources {
  def int(paths: String*): Config[Int]       = Config.Sources(PropertyType.IntType, paths)
  def double(paths: String*): Config[Double] = Config.Sources(PropertyType.DoubleType, paths)
  def string(paths: String*): Config[String] = Config.Sources(PropertyType.StringType, paths)
  def long(paths: String*): Config[Long]     = Config.Sources(PropertyType.LongType, paths)
  def short(paths: String*): Config[Short]   = Config.Sources(PropertyType.ShortType, paths)
  def uri(paths: String*): Config[URI]       = Config.Sources(PropertyType.UriType, paths)

  def opt[A](config: Config[A]): Config[Option[A]] =
    config match {
      case Config.Sources(propertyType, paths) => Config.Sources(PropertyType.ofOption(propertyType), paths)
      case Config.Source(path, propertyType)   => Config.Source(path, PropertyType.ofOption(propertyType))
      case a =>
        a.errorMap(a => Right(Some(a): Option[A]))(
          value =>
            value.fold[Either[WriteError, A]](Left(WriteError.apply("cannot obtain the value", None)))(t => Right(t))
        )
    }

  def list[A](config: Config[A]): Config[List[A]] =
    config match {
      case Config.Sources(propertyType, paths) => Config.Sources(PropertyType.ofList(propertyType), paths)
      case Config.Source(path, propertyType)   => Config.Source(path, PropertyType.ofList(propertyType))
      case a =>
        a.errorMap(a => Right(List(a): List[A]))(
          list =>
            list.headOption
              .fold[Either[WriteError, A]](Left(WriteError.apply("cannot obtain the value", None)))(t => Right(t))
        )
    }

  def read[A](config: Config[A]): Read[A]     = Read.read[A](config)
  def write[A](config: Config[A]): Write[A]   = Write.write[A](config)
  def report[A](config: Config[A]): Report[A] = Report.report[A](config)
}
