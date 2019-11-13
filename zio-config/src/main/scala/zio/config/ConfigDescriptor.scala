package zio.config

import java.net.URI

import zio.config.ReadErrors.ReadError

sealed trait ConfigDescriptor[+K, +V, A] { self =>
  final def zip[K1 >: K, V1 >: V, B](that: => ConfigDescriptor[K1, V1, B]): ConfigDescriptor[K1, V1, (A, B)] =
    ConfigDescriptor.Zip(self, that)

  final def <*>[K1 >: K, V1 >: V, B](that: => ConfigDescriptor[K1, V1, B]): ConfigDescriptor[K1, V1, (A, B)] =
    self.zip(that)

  final def xmapEither[K1 >: K, V1 >: V, B](
    f: A => Either[ReadError[K1, V1], B]
  )(g: B => Either[String, A]): ConfigDescriptor.XmapEither[K1, V1, A, B] =
    ConfigDescriptor.XmapEither(self, f, g)

  def xmapEither2[K1 >: K, V1 >: V, B, C](
    that: ConfigDescriptor[K1, V1, B]
  )(f: (A, B) => Either[ReadError[K1, V1], C])(g: C => Either[String, (A, B)]): ConfigDescriptor[K1, V1, C] =
    (self |@| that).apply[(A, B)](Tuple2.apply, Tuple2.unapply).xmapEither({ case (a, b) => f(a, b) })(g)

  final def xmap[B](to: A => B)(from: B => A): ConfigDescriptor[K, V, B] =
    self.xmapEither(a => Right(to(a)))(b => Right(from(b)))

  final def orElseEither[K1 >: K, V1 >: V, B](
    that: => ConfigDescriptor[K1, V1, B]
  ): ConfigDescriptor[K1, V1, Either[A, B]] =
    ConfigDescriptor.OrElseEither(self, that)

  final def <+>[K1 >: K, V1 >: V, B](that: => ConfigDescriptor[K1, V1, B]): ConfigDescriptor[K1, V1, Either[A, B]] =
    self orElseEither that

  def orElse[K1 >: K, V1 >: V](that: => ConfigDescriptor[K1, V1, A]): ConfigDescriptor[K1, V1, A] =
    (self orElseEither that).xmap {
      case Right(value) => value
      case Left(value)  => value
    }(b => Right(b))

  def |[K1 >: K, V1 >: V](that: => ConfigDescriptor[K1, V1, A]): ConfigDescriptor[K1, V1, A] =
    self orElse that

  def optional[K1 >: K, V1 >: V]: ConfigDescriptor[K1, V1, Option[A]] =
    ConfigDescriptor.Optional(self) ? "optional value"

  def default[K1 >: K, V1 >: V](value: A): ConfigDescriptor[K1, V1, A] =
    ConfigDescriptor.Default(self, value) ? s"default value: $value"

  def describe[K1 >: K, V1 >: V](description: String): ConfigDescriptor[K1, V1, A] =
    ConfigDescriptor.Describe(self, description)

  def ?[K1 >: K, V1 >: V](description: String): ConfigDescriptor[K1, V1, A] =
    describe(description)

  final def |@|[K1 >: K, V1 >: V, B](f: => ConfigDescriptor[K1, V1, B]): ProductBuilder[K1, V1, A, B] =
    new ProductBuilder[K1, V1, A, B] {
      override val a: ConfigDescriptor[K1, V1, A] = self
      override val b: ConfigDescriptor[K1, V1, B] = f
    }

  def from[K1 >: K, V1 >: V](configSource: ConfigSource[Vector[K1], V1]): ConfigDescriptor[K1, V1, A] =
    ??? // traverse and merge the source. `orElse`
  def fromNothing: ConfigDescriptor[K, V, A] = ??? // traverse through the struvture and replace source with empty

}

object ConfigDescriptor {

  final case class Empty[K, V, A]() extends ConfigDescriptor[K, V, Option[A]]

  final case class Source[K, V, A](path: K, propertyType: PropertyType[K, V, A], source: ConfigSource[Vector[K], V])
      extends ConfigDescriptor[K, V, A]

  final case class Nested[K, V, A](config: ConfigDescriptor[K, V, A], path: K) extends ConfigDescriptor[K, V, A]

  final case class Describe[K, V, A](config: ConfigDescriptor[K, V, A], message: String)
      extends ConfigDescriptor[K, V, A]

  final case class Default[K, V, A](config: ConfigDescriptor[K, V, A], value: A) extends ConfigDescriptor[K, V, A]

  final case class Optional[K, V, A](config: ConfigDescriptor[K, V, A]) extends ConfigDescriptor[K, V, Option[A]]

  final case class XmapEither[K, V, A, B](
    config: ConfigDescriptor[K, V, A],
    f: A => Either[ReadError[K, V], B],
    g: B => Either[String, A]
  ) extends ConfigDescriptor[K, V, B]

  final case class Zip[K, V, A, B](left: ConfigDescriptor[K, V, A], right: ConfigDescriptor[K, V, B])
      extends ConfigDescriptor[K, V, (A, B)]

  final case class OrElseEither[K, V, A, B](left: ConfigDescriptor[K, V, A], right: ConfigDescriptor[K, V, B])
      extends ConfigDescriptor[K, V, Either[A, B]]

  def empty[K, V, A]: ConfigDescriptor[K, V, Option[A]] = ConfigDescriptor.Empty()

  def sequence[K, V, A](configList: List[ConfigDescriptor[K, V, A]]): ConfigDescriptor[K, V, List[A]] =
    configList.foldLeft(Empty[K, V, A]().xmap(_.toList)(_.headOption))(
      (a, b) =>
        b.xmapEither2(a)((aa, bb) => Right(aa :: bb))(t => {
          t.headOption.fold[Either[String, (A, List[A])]](
            Left(
              "The input is not corresponding to the config description. It may have less number of entries than required by the config."
            )
          )(ll => Right((ll, t.tail)))
        })
    )

  def collectAll[K, V, A](configList: List[ConfigDescriptor[K, V, A]]): ConfigDescriptor[K, V, List[A]] =
    sequence(configList)

  def string(path: String): ConfigDescriptor[String, String, String] =
    ConfigDescriptor.Source(path, PropertyType.StringType, ConfigSource.empty) ? "value of type string"
  def boolean(path: String): ConfigDescriptor[String, String, Boolean] =
    ConfigDescriptor.Source(path, PropertyType.BooleanType, ConfigSource.empty) ? "value of type boolean"
  def byte(path: String): ConfigDescriptor[String, String, Byte] =
    ConfigDescriptor.Source(path, PropertyType.ByteType, ConfigSource.empty) ? "value of type byte"
  def short(path: String): ConfigDescriptor[String, String, Short] =
    ConfigDescriptor.Source(path, PropertyType.ShortType, ConfigSource.empty) ? "value of type short"
  def int(path: String): ConfigDescriptor[String, String, Int] =
    ConfigDescriptor.Source(path, PropertyType.IntType, ConfigSource.empty) ? "value of type int"
  def long(path: String): ConfigDescriptor[String, String, Long] =
    ConfigDescriptor.Source(path, PropertyType.LongType, ConfigSource.empty) ? "value of type long"
  def bigInt(path: String): ConfigDescriptor[String, String, BigInt] =
    ConfigDescriptor.Source(path, PropertyType.BigIntType, ConfigSource.empty) ? "value of type bigint"
  def float(path: String): ConfigDescriptor[String, String, Float] =
    ConfigDescriptor.Source(path, PropertyType.FloatType, ConfigSource.empty) ? "value of type float"
  def double(path: String): ConfigDescriptor[String, String, Double] =
    ConfigDescriptor.Source(path, PropertyType.DoubleType, ConfigSource.empty) ? "value of type double"
  def bigDecimal(path: String): ConfigDescriptor[String, String, BigDecimal] =
    ConfigDescriptor.Source(path, PropertyType.BigDecimalType, ConfigSource.empty) ? "value of type bigdecimal"
  def uri(path: String): ConfigDescriptor[String, String, URI] =
    ConfigDescriptor.Source(path, PropertyType.UriType, ConfigSource.empty) ? "value of type uri"
  def nested[K, V, A](path: K)(desc: ConfigDescriptor[K, V, A]): ConfigDescriptor[K, V, A] =
    ConfigDescriptor.Nested(desc, path)
}
