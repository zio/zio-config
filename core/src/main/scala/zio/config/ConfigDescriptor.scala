package zio.config

import scala.concurrent.duration.Duration

import java.net.URI
import zio.config.ConfigDescriptor._

sealed trait ConfigDescriptor[K, V, A] { self =>

  final def ??(description: String): ConfigDescriptor[K, V, A] =
    describe(description)

  final def |@|[B](f: => ConfigDescriptor[K, V, B]): ProductBuilder[K, V, A, B] =
    new ProductBuilder[K, V, A, B] {
      override val a: ConfigDescriptor[K, V, A] = self
      override val b: ConfigDescriptor[K, V, B] = f
    }

  final def <>(that: => ConfigDescriptor[K, V, A]): ConfigDescriptor[K, V, A] =
    self orElse that

  final def <*>[B](that: => ConfigDescriptor[K, V, B]): ConfigDescriptor[K, V, (A, B)] =
    self zip that

  final def <+>[B](that: => ConfigDescriptor[K, V, B]): ConfigDescriptor[K, V, Either[A, B]] =
    self orElseEither that

  final def default(value: A): ConfigDescriptor[K, V, A] =
    ConfigDescriptor.Default(self, value) ?? s"default value: $value"

  final def describe(description: String): ConfigDescriptor[K, V, A] =
    ConfigDescriptor.Describe(self, description)

  final def from(that: ConfigSource[K, V]): ConfigDescriptor[K, V, A] =
    self.updateSource(_.orElse(that))

  final def optional: ConfigDescriptor[K, V, Option[A]] =
    ConfigDescriptor.Optional(self) ?? "optional value"

  final def orElse(that: => ConfigDescriptor[K, V, A]): ConfigDescriptor[K, V, A] =
    ConfigDescriptor.OrElse(self, that)

  final def orElseEither[B](
    that: => ConfigDescriptor[K, V, B]
  ): ConfigDescriptor[K, V, Either[A, B]] =
    ConfigDescriptor.OrElseEither(self, that)

  final def unsourced: ConfigDescriptor[K, V, A] =
    self.updateSource(_ => ConfigSource.empty)

  final def updateSource(f: ConfigSource[K, V] => ConfigSource[K, V]): ConfigDescriptor[K, V, A] = {
    def loop[B](config: ConfigDescriptor[K, V, B]): ConfigDescriptor[K, V, B] = config match {
      case Source(path, source, propertyType) => Source(path, f(source), propertyType)
      case Nested(path, conf)                 => Nested(path, loop(conf))
      case Sequence(conf)                     => Sequence(loop(conf))
      case Describe(config, message)          => Describe(loop(config), message)
      case Default(value, value2)             => Default(loop(value), value2)
      case Optional(config)                   => Optional(loop(config))
      case XmapEither(config, f, g)           => XmapEither(loop(config), f, g)
      case Zip(conf1, conf2)                  => Zip(loop(conf1), loop(conf2))
      case OrElseEither(value1, value2)       => OrElseEither(loop(value1), loop(value2))
      case OrElse(value1, value2)             => OrElse(loop(value1), loop(value2))
    }
    loop(self)
  }

  final def zip[B](that: => ConfigDescriptor[K, V, B]): ConfigDescriptor[K, V, (A, B)] =
    ConfigDescriptor.Zip(self, that)

  final def xmapEither[B](
    f: A => Either[String, B]
  )(g: B => Either[String, A]): ConfigDescriptor[K, V, B] =
    ConfigDescriptor.XmapEither(self, f, g)

  def xmapEither2[B, C](
    that: ConfigDescriptor[K, V, B]
  )(f: (A, B) => Either[String, C])(g: C => Either[String, (A, B)]): ConfigDescriptor[K, V, C] =
    (self |@| that).apply[(A, B)](Tuple2.apply, Tuple2.unapply).xmapEither({ case (a, b) => f(a, b) })(g)

  final def xmap[B](to: A => B)(from: B => A): ConfigDescriptor[K, V, B] =
    self.xmapEither(a => Right(to(a)))(b => Right(from(b)))

  /**
   * This method exists to cover the corner case of a case class with only one field.
   * For example, a case class with two fields uses the `|@|` syntax to combine them:
   * {{{
   *   val cDoubleField: ConfigDescriptor[String, String, DoubleField] =
   *     (cId |@| cDbUrl)(DoubleField.apply, DoubleField.unapply)
   * }}}
   *
   * However, for a single field it is not clear what to do. For maximum similarity to
   * the multiple-field version, this method provides a syntax that matches what you
   * get by taking away the second field and the `|@|`:
   * {{{
   *   val cSingleField: ConfigDescriptor[String, String, SingleField] =
   *     int("cId")(SingleField.apply)(SingleField.unapply)
   * }}}
   */
  def apply[B](app: A => B)(unapp: B => Option[A]): ConfigDescriptor[K, V, B] =
    XmapEither(
      this,
      (a: A) => Right[String, B](app(a)),
      unapp(_)
        .fold[Either[String, A]](Left("Unable to create case class instance"))(Right(_))
    )
}

object ConfigDescriptor {

  final case class Default[K, V, A](config: ConfigDescriptor[K, V, A], value: A) extends ConfigDescriptor[K, V, A]

  final case class Describe[K, V, A](config: ConfigDescriptor[K, V, A], message: String)
      extends ConfigDescriptor[K, V, A]

  final case class Nested[K, V, A](path: K, config: ConfigDescriptor[K, V, A]) extends ConfigDescriptor[K, V, A]

  final case class Optional[K, V, A](config: ConfigDescriptor[K, V, A]) extends ConfigDescriptor[K, V, Option[A]]

  final case class OrElse[K, V, A](left: ConfigDescriptor[K, V, A], right: ConfigDescriptor[K, V, A])
      extends ConfigDescriptor[K, V, A]

  final case class OrElseEither[K, V, A, B](left: ConfigDescriptor[K, V, A], right: ConfigDescriptor[K, V, B])
      extends ConfigDescriptor[K, V, Either[A, B]]

  final case class Sequence[K, V, A](config: ConfigDescriptor[K, V, A]) extends ConfigDescriptor[K, V, List[A]]

  final case class Source[K, V, A](path: K, source: ConfigSource[K, V], propertyType: PropertyType[V, A])
      extends ConfigDescriptor[K, V, A]

  final case class Zip[K, V, A, B](left: ConfigDescriptor[K, V, A], right: ConfigDescriptor[K, V, B])
      extends ConfigDescriptor[K, V, (A, B)]

  final case class XmapEither[K, V, A, B](
    config: ConfigDescriptor[K, V, A],
    f: A => Either[String, B],
    g: B => Either[String, A]
  ) extends ConfigDescriptor[K, V, B]

  def bigDecimal(path: String): ConfigDescriptor[String, String, BigDecimal] =
    ConfigDescriptor.Source(path, ConfigSource.empty, PropertyType.BigDecimalType) ?? "value of type bigdecimal"

  def bigInt(path: String): ConfigDescriptor[String, String, BigInt] =
    ConfigDescriptor.Source(path, ConfigSource.empty, PropertyType.BigIntType) ?? "value of type bigint"

  def boolean(path: String): ConfigDescriptor[String, String, Boolean] =
    ConfigDescriptor.Source(path, ConfigSource.empty, PropertyType.BooleanType) ?? "value of type boolean"

  def byte(path: String): ConfigDescriptor[String, String, Byte] =
    ConfigDescriptor.Source(path, ConfigSource.empty, PropertyType.ByteType) ?? "value of type byte"

  def collectAll[K, V, A](configList: ::[ConfigDescriptor[K, V, A]]): ConfigDescriptor[K, V, ::[A]] =
    sequence(configList)

  def double(path: String): ConfigDescriptor[String, String, Double] =
    ConfigDescriptor.Source(path, ConfigSource.empty, PropertyType.DoubleType) ?? "value of type double"

  def duration(path: String): ConfigDescriptor[String, String, Duration] =
    ConfigDescriptor.Source(path, ConfigSource.empty, PropertyType.DurationType) ?? "value of type duration"

  def float(path: String): ConfigDescriptor[String, String, Float] =
    ConfigDescriptor.Source(path, ConfigSource.empty, PropertyType.FloatType) ?? "value of type float"

  def int(path: String): ConfigDescriptor[String, String, Int] =
    ConfigDescriptor.Source(path, ConfigSource.empty, PropertyType.IntType) ?? "value of type int"

  def list[K, V, A](desc: ConfigDescriptor[K, V, A]): ConfigDescriptor[K, V, List[A]] =
    ConfigDescriptor.Sequence(desc)

  def long(path: String): ConfigDescriptor[String, String, Long] =
    ConfigDescriptor.Source(path, ConfigSource.empty, PropertyType.LongType) ?? "value of type long"

  def nested[K, V, A](path: K)(desc: ConfigDescriptor[K, V, A]): ConfigDescriptor[K, V, A] =
    ConfigDescriptor.Nested(path, desc)

  def sequence[K, V, A](configList: ::[ConfigDescriptor[K, V, A]]): ConfigDescriptor[K, V, ::[A]] = {
    val reversed = configList.reverse
    reversed.tail.foldLeft(
      reversed.head.xmap(a => ::(a, Nil))(b => b.head)
    )(
      (b, a) =>
        b.xmapEither2(a)((as, a) => {
          Right(::(a, as))
        })(
          t => Right((::(t.tail.head, t.tail.tail), t.head))
        )
    )
  }

  def short(path: String): ConfigDescriptor[String, String, Short] =
    ConfigDescriptor.Source(path, ConfigSource.empty, PropertyType.ShortType) ?? "value of type short"

  def string(path: String): ConfigDescriptor[String, String, String] =
    ConfigDescriptor.Source(path, ConfigSource.empty, PropertyType.StringType) ?? "value of type string"

  def uri(path: String): ConfigDescriptor[String, String, URI] =
    ConfigDescriptor.Source(path, ConfigSource.empty, PropertyType.UriType) ?? "value of type uri"
}
