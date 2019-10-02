package zio

import java.net.URI

import zio.config.actions.{ ConfigDescription, Read, Write }

package object config extends Sources {
  def string(path: String): ConfigDescriptor[String] =
    ConfigDescriptor.Source(path, PropertyType.StringType) ~ "value of type string"
  def boolean(path: String): ConfigDescriptor[Boolean] =
    ConfigDescriptor.Source(path, PropertyType.BooleanType) ~ "value of type boolean"
  def byte(path: String): ConfigDescriptor[Byte] =
    ConfigDescriptor.Source(path, PropertyType.ByteType) ~ "value of type byte"
  def short(path: String): ConfigDescriptor[Short] =
    ConfigDescriptor.Source(path, PropertyType.ShortType) ~ "value of type short"
  def int(path: String): ConfigDescriptor[Int] =
    ConfigDescriptor.Source(path, PropertyType.IntType) ~ "value of type int"
  def long(path: String): ConfigDescriptor[Long] =
    ConfigDescriptor.Source(path, PropertyType.LongType) ~ "value of type long"
  def bigInt(path: String): ConfigDescriptor[BigInt] =
    ConfigDescriptor.Source(path, PropertyType.BigIntType) ~ "value of type bigint"
  def float(path: String): ConfigDescriptor[Float] =
    ConfigDescriptor.Source(path, PropertyType.FloatType) ~ "value of type float"
  def double(path: String): ConfigDescriptor[Double] =
    ConfigDescriptor.Source(path, PropertyType.DoubleType) ~ "value of type double"
  def bigDecimal(path: String): ConfigDescriptor[BigDecimal] =
    ConfigDescriptor.Source(path, PropertyType.BigDecimalType) ~ "value of type bigdecimal"
  def uri(path: String): ConfigDescriptor[URI] =
    ConfigDescriptor.Source(path, PropertyType.UriType) ~ "value of type uri"

  def read[A](config: => ConfigDescriptor[A]): ZIO[ConfigSource, ReadErrors, (ConfigReport, A)] = Read.read[A](config)
  def reportOfProvidedConfig[A](config: => ConfigDescriptor[A]): ZIO[ConfigSource, ReadErrors, ConfigReport] =
    read(config).map(_._1)
  def write[A](config: => ConfigDescriptor[A]): Write[A]            = Write.write[A](config)
  def manPage[A](config: => ConfigDescriptor[A]): ConfigDescription = ConfigDescription.man[A](config)

  def getConfigValue(path: String): ZIO[ConfigSource, Unit, String] = ZIO.accessM(_.configService.getConfigValue(path))

  type ReadErrors = ::[ReadError]

  object ReadErrors {
    def apply(a: ReadError, as: ReadError*): ReadErrors =
      ::(a, as.toList)

    def concat(l: ReadErrors, r: ReadErrors): ReadErrors =
      ::(l.head, l.tail ++ r)
  }
}
