package zio

import zio.config.actions.{ Read, Report, Write }

package object config extends ConfigSyntax with Sources {
  def int(paths: String*): Config[Int]       = Config.Sources(PropertyType.IntType, paths)
  def double(paths: String*): Config[Double] = Config.Sources(PropertyType.DoubleType, paths)
  def string(paths: String*): Config[String] = Config.Sources(PropertyType.StringType, paths)
  def long(paths: String*): Config[Long]     = Config.Sources(PropertyType.LongType, paths)

  def opt[A](config: Config[A]): Config[Option[A]] =
    config match {
      case Config.Sources(propertyType, paths) => Config.Sources(PropertyType.ofOption(propertyType), paths)
      case Config.Source(path, propertyType)   => Config.Source(path, PropertyType.ofOption(propertyType))
      case a                                   => a.map(t => Some(t))
    }

  def list[A](config: Config[A]): Config[List[A]] =
    config match {
      case Config.Sources(propertyType, paths) => Config.Sources(PropertyType.ofList(propertyType), paths)
      case Config.Source(path, propertyType)   => Config.Source(path, PropertyType.ofList(propertyType))
      case a                                   => list(a)
    }

  def read[A](config: Config[A]): Read[A]     = Read.read[A](config)
  def write[A](config: Config[A]): Write[A]   = Write.write[A](config)
  def report[A](config: Config[A]): Report[A] = Report.report[A](config)
}
