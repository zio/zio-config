package zio.config

import java.net.URI
import zio.config.actions.Read
import zio.system.System
import zio.{ IO, UIO, ZIO }

trait Config[A] {
  def config: Config.Service[A]
}
object Config {
  trait Service[A] {
    def config: UIO[A]
  }

  def make[A](
    source: ConfigSource[String, String],
    configDescriptor: ConfigDescriptor[A]
  ): IO[ReadErrors[String, String], Config[A]] =
    Read
      .read(configDescriptor)
      .provide(source)
      .map(
        e =>
          new Config[A] {
            override def config: Service[A] = new Service[A] {
              override def config: UIO[A] = ZIO.succeed(e)
            }
          }
      )

  def fromEnv[A](configDescriptor: ConfigDescriptor[A]): ZIO[System, ReadErrors[String, String], Config[A]] =
    for {
      source <- ConfigSource.fromEnv
      res    <- make(source, configDescriptor)
    } yield res

  def fromMap[A](
    map: Map[String, String],
    configDescriptor: ConfigDescriptor[A]
  ): IO[ReadErrors[String, String], Config[A]] =
    make(ConfigSource.fromMap(map), configDescriptor)

  def fromPropertyFile[A](configDescriptor: ConfigDescriptor[A]): ZIO[System, ReadErrors[String, String], Config[A]] =
    for {
      source <- ConfigSource.fromProperty
      res    <- make(source, configDescriptor)
    } yield res

  def string(path: String): ConfigDescriptor[String] =
    ConfigDescriptor.Source(path, PropertyType.StringType) ? "value of type string"
  def boolean(path: String): ConfigDescriptor[Boolean] =
    ConfigDescriptor.Source(path, PropertyType.BooleanType) ? "value of type boolean"
  def byte(path: String): ConfigDescriptor[Byte] =
    ConfigDescriptor.Source(path, PropertyType.ByteType) ? "value of type byte"
  def short(path: String): ConfigDescriptor[Short] =
    ConfigDescriptor.Source(path, PropertyType.ShortType) ? "value of type short"
  def int(path: String): ConfigDescriptor[Int] =
    ConfigDescriptor.Source(path, PropertyType.IntType) ? "value of type int"
  def long(path: String): ConfigDescriptor[Long] =
    ConfigDescriptor.Source(path, PropertyType.LongType) ? "value of type long"
  def bigInt(path: String): ConfigDescriptor[BigInt] =
    ConfigDescriptor.Source(path, PropertyType.BigIntType) ? "value of type bigint"
  def float(path: String): ConfigDescriptor[Float] =
    ConfigDescriptor.Source(path, PropertyType.FloatType) ? "value of type float"
  def double(path: String): ConfigDescriptor[Double] =
    ConfigDescriptor.Source(path, PropertyType.DoubleType) ? "value of type double"
  def bigDecimal(path: String): ConfigDescriptor[BigDecimal] =
    ConfigDescriptor.Source(path, PropertyType.BigDecimalType) ? "value of type bigdecimal"
  def uri(path: String): ConfigDescriptor[URI] =
    ConfigDescriptor.Source(path, PropertyType.UriType) ? "value of type uri"
  def nested[A](path: String)(desc: ConfigDescriptor[A]): ConfigDescriptor[A] =
    ConfigDescriptor.Nested(desc, path)
}
