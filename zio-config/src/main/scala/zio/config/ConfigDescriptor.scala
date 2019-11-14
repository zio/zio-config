package zio.config

import java.net.URI

sealed trait ConfigDescriptor[K, V, A] { self =>
  final def zip[B](that: => ConfigDescriptor[K, V, B]): ConfigDescriptor[K, V, (A, B)] =
    ConfigDescriptor.Zip(self, that)

  final def <*>[B](that: => ConfigDescriptor[K, V, B]): ConfigDescriptor[K, V, (A, B)] =
    self.zip(that)

  final def xmapEither[B](
    f: A => Either[String, B]
  )(g: B => Either[String, A]): ConfigDescriptor.XmapEither[K, V, A, B] =
    ConfigDescriptor.XmapEither(self, f, g)

  def xmapEither2[B, C](
    that: ConfigDescriptor[K, V, B]
  )(f: (A, B) => Either[String, C])(g: C => Either[String, (A, B)]): ConfigDescriptor[K, V, C] =
    (self |@| that).apply[(A, B)](Tuple2.apply, Tuple2.unapply).xmapEither({ case (a, b) => f(a, b) })(g)

  final def xmap[B](to: A => B)(from: B => A): ConfigDescriptor[K, V, B] =
    self.xmapEither(a => Right(to(a)))(b => Right(from(b)))

  final def orElseEither[B](
    that: => ConfigDescriptor[K, V, B]
  ): ConfigDescriptor[K, V, Either[A, B]] =
    ConfigDescriptor.OrElseEither(self, that)

  final def <+>[B](that: => ConfigDescriptor[K, V, B]): ConfigDescriptor[K, V, Either[A, B]] =
    self orElseEither that

  def orElse(that: => ConfigDescriptor[K, V, A]): ConfigDescriptor[K, V, A] =
    (self orElseEither that).xmap {
      case Right(value) => value
      case Left(value)  => value
    }(b => Right(b))

  def |(that: => ConfigDescriptor[K, V, A]): ConfigDescriptor[K, V, A] =
    self orElse that

  def optional: ConfigDescriptor[K, V, Option[A]] =
    ConfigDescriptor.Optional(self) ? "optional value"

  def default(value: A): ConfigDescriptor[K, V, A] =
    ConfigDescriptor.Default(self, value) ? s"default value: $value"

  def describe(description: String): ConfigDescriptor[K, V, A] =
    ConfigDescriptor.Describe(self, description)

  def ?(description: String): ConfigDescriptor[K, V, A] =
    describe(description)

  final def |@|[B](f: => ConfigDescriptor[K, V, B]): ProductBuilder[K, V, A, B] =
    new ProductBuilder[K, V, A, B] {
      override val a: ConfigDescriptor[K, V, A] = self
      override val b: ConfigDescriptor[K, V, B] = f
    }

  def from(configSource: ConfigSource[K, V]): ConfigDescriptor[K, V, A] =
    ??? // traverse and merge the source. `orElse`
  def fromNothing: ConfigDescriptor[K, V, A] = ??? // traverse through the struvture and replace source with empty

}

object ConfigDescriptor {

  final case class Empty[K, V, A]() extends ConfigDescriptor[K, V, Option[A]]

  final case class Source[K, V, A](path: K, source: ConfigSource[K, V], propertyType: PropertyType[V, A])
    extends ConfigDescriptor[K, V, A]

  final case class Nested[K, V, A](config: ConfigDescriptor[K, V, A], path: K)
    extends ConfigDescriptor[K, V, A]

  final case class Describe[K, V, A](config: ConfigDescriptor[K, V, A], message: String)
    extends ConfigDescriptor[K, V, A]

  final case class Default[K, V, A](config: ConfigDescriptor[K, V, A], value: A)
    extends ConfigDescriptor[K, V, A]

  final case class Optional[K, V, A](config: ConfigDescriptor[K, V, A])
    extends ConfigDescriptor[K, V, Option[A]]

  final case class XmapEither[K, V, A, B](
    config: ConfigDescriptor[K, V, A],
    f: A => Either[String, B],
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
    ConfigDescriptor.Source(path, ConfigSource.empty, PropertyType.StringType) ? "value of type string"
  def boolean(path: String): ConfigDescriptor[String, String, Boolean] =
    ConfigDescriptor.Source(path, ConfigSource.empty, PropertyType.BooleanType) ? "value of type boolean"
  def byte(path: String): ConfigDescriptor[String, String, Byte] =
    ConfigDescriptor.Source(path, ConfigSource.empty, PropertyType.ByteType) ? "value of type byte"
  def short(path: String): ConfigDescriptor[String, String, Short] =
    ConfigDescriptor.Source(path, ConfigSource.empty, PropertyType.ShortType) ? "value of type short"
  def int(path: String): ConfigDescriptor[String, String, Int] =
    ConfigDescriptor.Source(path, ConfigSource.empty, PropertyType.IntType) ? "value of type int"
  def long(path: String): ConfigDescriptor[String, String, Long] =
    ConfigDescriptor.Source(path, ConfigSource.empty, PropertyType.LongType) ? "value of type long"
  def bigInt(path: String): ConfigDescriptor[String, String, BigInt] =
    ConfigDescriptor.Source(path, ConfigSource.empty, PropertyType.BigIntType) ? "value of type bigint"
  def float(path: String): ConfigDescriptor[String, String, Float] =
    ConfigDescriptor.Source(path, ConfigSource.empty, PropertyType.FloatType) ? "value of type float"
  def double(path: String): ConfigDescriptor[String, String, Double] =
    ConfigDescriptor.Source(path, ConfigSource.empty, PropertyType.DoubleType) ? "value of type double"
  def bigDecimal(path: String): ConfigDescriptor[String, String, BigDecimal] =
    ConfigDescriptor.Source(path, ConfigSource.empty, PropertyType.BigDecimalType) ? "value of type bigdecimal"
  def uri(path: String): ConfigDescriptor[String, String, URI] =
    ConfigDescriptor.Source(path, ConfigSource.empty, PropertyType.UriType) ? "value of type uri"
  def nested[K, V, A](path: K)(desc: ConfigDescriptor[K, V, A]): ConfigDescriptor[K, V, A] =
    ConfigDescriptor.Nested(desc, path)
}
