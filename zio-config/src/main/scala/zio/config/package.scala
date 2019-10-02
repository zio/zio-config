package zio

import java.net.URI

import zio.config.actions.{ Read, Report, Write }

package object config extends Sources {
  def string(path: String): Config[String]         = Config.Source(path, PropertyType.StringType)
  def boolean(path: String): Config[Boolean]       = Config.Source(path, PropertyType.BooleanType)
  def byte(path: String): Config[Byte]             = Config.Source(path, PropertyType.ByteType)
  def short(path: String): Config[Short]           = Config.Source(path, PropertyType.ShortType)
  def int(path: String): Config[Int]               = Config.Source(path, PropertyType.IntType)
  def long(path: String): Config[Long]             = Config.Source(path, PropertyType.LongType)
  def bigInt(path: String): Config[BigInt]         = Config.Source(path, PropertyType.BigIntType)
  def float(path: String): Config[Float]           = Config.Source(path, PropertyType.FloatType)
  def double(path: String): Config[Double]         = Config.Source(path, PropertyType.DoubleType)
  def bigDecimal(path: String): Config[BigDecimal] = Config.Source(path, PropertyType.BigDecimalType)
  def uri(path: String): Config[URI]               = Config.Source(path, PropertyType.UriType)

  def opt[A](config: => Config[A]): Config[Option[A]] =
    config
      .xmapEither[Option[A]](a => Right(Some(a)))({
        case Some(value) => Right(value)
        case None        => Left("Error: Cannot write a none value")
      })
      .onError(_ => None)

  def read[A](config: => Config[A]): Read[A]     = Read.read[A](config)
  def write[A](config: => Config[A]): Write[A]   = Write.write[A](config)
  def report[A](config: => Config[A]): Report[A] = Report.report[A](config)

  def getConfigValue(path: String): ZIO[ConfigSource, Unit, String] = ZIO.accessM(_.configService.getConfigValue(path))

  ////

  type ReadErrors = ::[ReadError]

  object ReadErrors {
    def apply(a: ReadError, as: ReadError*): ReadErrors =
      ::(a, as.toList)

    def concat(l: ReadErrors, r: ReadErrors): ReadErrors =
      ::(l.head, l.tail ++ r)
  }
}
