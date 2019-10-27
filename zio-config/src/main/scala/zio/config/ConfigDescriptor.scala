package zio.config

import java.net.URI

import zio.config.ReadErrors.ReadError

sealed trait ConfigDescriptor[+K, +V, A] { self =>
  final def zip[K1 >: K, B](that: => ConfigDescriptor[K1, B]): ConfigDescriptor[(A, B)] =
    ConfigDescriptor.Zip(self, that)

  final def <*>[B](that: => ConfigDescriptor[B]): ConfigDescriptor[(A, B)] =
    self.zip(that)

  final def xmapEither[B](
    f: A => Either[ReadError[String, String], B]
  )(g: B => Either[String, A]): ConfigDescriptor.XmapEither[A, B] =
    ConfigDescriptor.XmapEither(self, f, g)

  def xmapEither2[B, C](
    that: ConfigDescriptor[B]
  )(f: (A, B) => Either[ReadError[String, String], C])(g: C => Either[String, (A, B)]): ConfigDescriptor[C] =
    (self |@| that).apply[(A, B)](Tuple2.apply, Tuple2.unapply).xmapEither({ case (a, b) => f(a, b) })(g)

  final def xmap[B](to: A => B)(from: B => A): ConfigDescriptor[B] =
    self.xmapEither(a => Right(to(a)))(b => Right(from(b)))

  final def orElseEither[B](that: => ConfigDescriptor[B]): ConfigDescriptor[Either[A, B]] =
    ConfigDescriptor.OrElseEither(self, that)

  final def <+>[B](that: => ConfigDescriptor[B]): ConfigDescriptor[Either[A, B]] =
    self orElseEither that

  def orElse(that: => ConfigDescriptor[A]): ConfigDescriptor[A] =
    (self orElseEither that).xmap {
      case Right(value) => value
      case Left(value)  => value
    }(b => Right(b))

  def |(that: => ConfigDescriptor[A]): ConfigDescriptor[A] =
    self orElse that

  def optional: ConfigDescriptor[Option[A]] =
    ConfigDescriptor.Optional(self) ? "optional value"

  def default(value: A): ConfigDescriptor[A] =
    ConfigDescriptor.Default(self, value) ? s"default value: $value"

  def describe(description: String): ConfigDescriptor[A] =
    ConfigDescriptor.Describe(self, description)

  def ?(description: String): ConfigDescriptor[A] =
    describe(description)

  final def |@|[B](f: => ConfigDescriptor[B]): ProductBuilder[A, B] =
    new ProductBuilder[A, B] {
      override val a: ConfigDescriptor[A] = self
      override val b: ConfigDescriptor[B] = f
    }

  def from[K1 >: K, V1 >: V](configSource: ConfigSource[K1, V1]): ConfigDescriptor[K1, V1, A] = ??? // traverse and merge the source. `orElse`
  def fromNothing: ConfigDescriptor[K, V, A] = ??? // traverse through the struvture and replace source with empty

}

object ConfigDescriptor {

  final case class Empty[A]() extends ConfigDescriptor[Option[A]]

  final case class Source[K, V, A](path: K, propertyType: PropertyType[K, V, A], source: ConfigSource[K, V]) extends ConfigDescriptor[A]

  final case class Nested[K, A](config: ConfigDescriptor[A], path: K) extends ConfigDescriptor[A]

  final case class Describe[A](config: ConfigDescriptor[A], message: String) extends ConfigDescriptor[A]

  final case class Default[A](config: ConfigDescriptor[A], value: A) extends ConfigDescriptor[A]

  final case class Optional[A](config: ConfigDescriptor[A]) extends ConfigDescriptor[Option[A]]

  final case class XmapEither[A, B](
    config: ConfigDescriptor[A],
    f: A => Either[ReadError[String, String], B],
    g: B => Either[String, A]
  ) extends ConfigDescriptor[B]

  final case class Zip[A, B](left: ConfigDescriptor[A], right: ConfigDescriptor[B]) extends ConfigDescriptor[(A, B)]

  final case class OrElseEither[A, B](left: ConfigDescriptor[A], right: ConfigDescriptor[B])
      extends ConfigDescriptor[Either[A, B]]

  def empty[A]: ConfigDescriptor[Option[A]] = ConfigDescriptor.Empty()

  def sequence[A](configList: List[ConfigDescriptor[A]]): ConfigDescriptor[List[A]] =
    configList.foldLeft(Empty[A]().xmap(_.toList)(_.headOption))(
      (a, b) =>
        b.xmapEither2(a)((aa, bb) => Right(aa :: bb))(t => {
          t.headOption.fold[Either[String, (A, List[A])]](
            Left(
              "The input is not corresponding to the config description. It may have less number of entries than required by the config."
            )
          )(ll => Right((ll, t.tail)))
        })
    )

  def collectAll[A](configList: List[ConfigDescriptor[A]]): ConfigDescriptor[List[A]] =
    sequence(configList)

  def string(path: String): ConfigDescriptor[String, String, String] =
    ConfigDescriptor.Source(path, PropertyType.StringType, ConfigSource.empty) ? "value of type string"
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
