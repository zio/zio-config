package zio.config

import java.io.File

import scala.concurrent.duration.Duration
import java.net.{ URI, URL }
import java.time.{ Instant, LocalDate, LocalDateTime, LocalTime }
import java.util.UUID

import zio.config.ConfigDescriptor._

sealed trait ConfigDescriptor[K, V, A] { self =>
  def apply[B](app: A => B, unapp: B => Option[A]): ConfigDescriptor[K, V, B] =
    XmapEither(
      this,
      (a: A) => Right[String, B](app(a)),
      unapp(_)
        .fold[Either[String, A]](Left("Unable to create case class instance"))(Right(_))
    )

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

  def mapKey(f: K => K): ConfigDescriptor[K, V, A] = {
    def loop[B](config: ConfigDescriptor[K, V, B]): ConfigDescriptor[K, V, B] = config match {
      case Source(source, propertyType) => Source(source, propertyType)
      case DynamicMap(source, conf)     => DynamicMap(source, loop(conf))
      case Nested(path, conf)           => Nested(f(path), loop(conf))
      case Sequence(source, conf)       => Sequence(source, loop(conf))
      case Describe(config, message)    => Describe(loop(config), message)
      case Default(value, value2)       => Default(loop(value), value2)
      case Optional(config)             => Optional(loop(config))
      case XmapEither(config, f, g)     => XmapEither(loop(config), f, g)
      case Zip(conf1, conf2)            => Zip(loop(conf1), loop(conf2))
      case OrElseEither(value1, value2) => OrElseEither(loop(value1), loop(value2))
      case OrElse(value1, value2)       => OrElse(loop(value1), loop(value2))
    }
    loop(self)
  }

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
      case Source(source, propertyType) => Source(f(source), propertyType)
      case DynamicMap(source, conf)     => DynamicMap(f(source), loop(conf))
      case Nested(path, conf)           => Nested(path, loop(conf))
      case Sequence(source, conf)       => Sequence(f(source), loop(conf))
      case Describe(config, message)    => Describe(loop(config), message)
      case Default(value, value2)       => Default(loop(value), value2)
      case Optional(config)             => Optional(loop(config))
      case XmapEither(config, f, g)     => XmapEither(loop(config), f, g)
      case Zip(conf1, conf2)            => Zip(loop(conf1), loop(conf2))
      case OrElseEither(value1, value2) => OrElseEither(loop(value1), loop(value2))
      case OrElse(value1, value2)       => OrElse(loop(value1), loop(value2))
    }
    loop(self)
  }

  final def xmap[B](to: A => B, from: B => A): ConfigDescriptor[K, V, B] =
    self.xmapEither(a => Right(to(a)), b => Right(from(b)))

  final def xmapEither[B](f: A => Either[String, B], g: B => Either[String, A]): ConfigDescriptor[K, V, B] =
    ConfigDescriptor.XmapEither(self, f, g)

  def xmapEither2[B, C](
    that: ConfigDescriptor[K, V, B]
  )(f: (A, B) => Either[String, C], g: C => Either[String, (A, B)]): ConfigDescriptor[K, V, C] =
    (self |@| that)
      .apply[(A, B)](Tuple2.apply, Tuple2.unapply)
      .xmapEither({ case (a, b) => f(a, b) }, g)

  final def zip[B](that: => ConfigDescriptor[K, V, B]): ConfigDescriptor[K, V, (A, B)] =
    ConfigDescriptor.Zip(self, that)
}

object ConfigDescriptor {
  final case class Default[K, V, A](config: ConfigDescriptor[K, V, A], value: A) extends ConfigDescriptor[K, V, A]

  final case class Describe[K, V, A](config: ConfigDescriptor[K, V, A], message: String)
      extends ConfigDescriptor[K, V, A]

  final case class DynamicMap[K, V, A](source: ConfigSource[K, V], config: ConfigDescriptor[K, V, A])
      extends ConfigDescriptor[K, V, Map[K, A]]

  final case class Nested[K, V, A](path: K, config: ConfigDescriptor[K, V, A]) extends ConfigDescriptor[K, V, A]

  final case class Optional[K, V, A](config: ConfigDescriptor[K, V, A]) extends ConfigDescriptor[K, V, Option[A]]

  final case class OrElse[K, V, A](left: ConfigDescriptor[K, V, A], right: ConfigDescriptor[K, V, A])
      extends ConfigDescriptor[K, V, A]

  final case class OrElseEither[K, V, A, B](left: ConfigDescriptor[K, V, A], right: ConfigDescriptor[K, V, B])
      extends ConfigDescriptor[K, V, Either[A, B]]

  final case class Sequence[K, V, A](source: ConfigSource[K, V], config: ConfigDescriptor[K, V, A])
      extends ConfigDescriptor[K, V, List[A]]

  final case class Source[K, V, A](source: ConfigSource[K, V], propertyType: PropertyType[V, A])
      extends ConfigDescriptor[K, V, A]

  final case class Zip[K, V, A, B](left: ConfigDescriptor[K, V, A], right: ConfigDescriptor[K, V, B])
      extends ConfigDescriptor[K, V, (A, B)]

  final case class XmapEither[K, V, A, B](
    config: ConfigDescriptor[K, V, A],
    f: A => Either[String, B],
    g: B => Either[String, A]
  ) extends ConfigDescriptor[K, V, B]

  val bigDecimal: ConfigDescriptor[String, String, BigDecimal] =
    ConfigDescriptor.Source(ConfigSource.empty, PropertyType.BigDecimalType) ?? "value of type bigdecimal"

  def bigDecimal(path: String): ConfigDescriptor[String, String, BigDecimal] = nested(path)(bigDecimal)

  val bigInt: ConfigDescriptor[String, String, BigInt] =
    ConfigDescriptor.Source(ConfigSource.empty, PropertyType.BigIntType) ?? "value of type bigint"

  def bigInt(path: String): ConfigDescriptor[String, String, BigInt] = nested(path)(bigInt)

  val boolean: ConfigDescriptor[String, String, Boolean] =
    ConfigDescriptor.Source(ConfigSource.empty, PropertyType.BooleanType) ?? "value of type boolean"

  def boolean(path: String): ConfigDescriptor[String, String, Boolean] = nested(path)(boolean)

  val byte: ConfigDescriptor[String, String, Byte] =
    ConfigDescriptor.Source(ConfigSource.empty, PropertyType.ByteType) ?? "value of type byte"

  def byte(path: String): ConfigDescriptor[String, String, Byte] = nested(path)(byte)

  def collectAll[K, V, A](
    head: ConfigDescriptor[K, V, A],
    tail: ConfigDescriptor[K, V, A]*
  ): ConfigDescriptor[K, V, List[A]] =
    sequence(head, tail: _*)({ case (a, t) => a :: t }, l => l.headOption.map(h => (h, l.tail)))

  val double: ConfigDescriptor[String, String, Double] =
    ConfigDescriptor.Source(ConfigSource.empty, PropertyType.DoubleType) ?? "value of type double"

  def double(path: String): ConfigDescriptor[String, String, Double] = nested(path)(double)

  val duration: ConfigDescriptor[String, String, Duration] =
    ConfigDescriptor.Source(ConfigSource.empty, PropertyType.DurationType) ?? "value of type duration"

  def duration(path: String): ConfigDescriptor[String, String, Duration] = nested(path)(duration)

  val zioDuration: ConfigDescriptor[String, String, zio.duration.Duration] =
    ConfigDescriptor.Source(ConfigSource.empty, PropertyType.ZioDurationType) ?? "value of type duration"

  def zioDuration(path: String): ConfigDescriptor[String, String, zio.duration.Duration] = nested(path)(zioDuration)

  val float: ConfigDescriptor[String, String, Float] =
    ConfigDescriptor.Source(ConfigSource.empty, PropertyType.FloatType) ?? "value of type float"

  def head[K, V, A](desc: ConfigDescriptor[K, V, A]): ConfigDescriptor[K, V, A] =
    desc.orElse(
      listStrict(desc)
        .xmapEither[A](_.headOption.fold[Either[String, A]](Left("Element is missing"))(Right(_)), v => Right(v :: Nil))
    )

  def head[K, V, A](path: K)(desc: ConfigDescriptor[K, V, A]): ConfigDescriptor[K, V, A] =
    nested(path)(head(desc))

  def float(path: String): ConfigDescriptor[String, String, Float] = nested(path)(float)

  val int: ConfigDescriptor[String, String, Int] =
    ConfigDescriptor.Source(ConfigSource.empty, PropertyType.IntType) ?? "value of type int"

  def int(path: String): ConfigDescriptor[String, String, Int] = nested(path)(int)

  // Fixme: Add detailed API docs
  def list[K, V, A](desc: ConfigDescriptor[K, V, A]): ConfigDescriptor[K, V, List[A]] =
    listStrict(desc).orElse(desc.apply(_ :: Nil, _.headOption))

  def list[K, V, A](path: K)(desc: ConfigDescriptor[K, V, A]): ConfigDescriptor[K, V, List[A]] =
    nested(path)(list(desc))

  def listStrict[K, V, A](desc: ConfigDescriptor[K, V, A]): ConfigDescriptor[K, V, List[A]] =
    ConfigDescriptor.Sequence(ConfigSource.empty, desc)

  def listStrict[K, V, A](path: K)(desc: ConfigDescriptor[K, V, A]): ConfigDescriptor[K, V, List[A]] =
    nested(path)(listStrict(desc))

  val long: ConfigDescriptor[String, String, Long] =
    ConfigDescriptor.Source(ConfigSource.empty, PropertyType.LongType) ?? "value of type long"

  def long(path: String): ConfigDescriptor[String, String, Long] = nested(path)(long)

  def map[K, V, A](desc: ConfigDescriptor[K, V, A]): ConfigDescriptor[K, V, Map[K, A]] =
    mapStrict[K, V, A](desc)

  def map[K, V, A](path: K)(desc: ConfigDescriptor[K, V, A]): ConfigDescriptor[K, V, Map[K, A]] =
    nested(path)(mapStrict(desc))

  def mapStrict[K, V, A](desc: ConfigDescriptor[K, V, A]): ConfigDescriptor[K, V, Map[K, A]] =
    ConfigDescriptor.DynamicMap(ConfigSource.empty, desc)

  def nested[K, V, A](path: K)(desc: ConfigDescriptor[K, V, A]): ConfigDescriptor[K, V, A] =
    ConfigDescriptor.Nested(path, desc)

  def sequence[K, V, A](
    head: ConfigDescriptor[K, V, A],
    tail: ConfigDescriptor[K, V, A]*
  ): ConfigDescriptor[K, V, (A, List[A])] =
    tail.reverse.foldLeft[ConfigDescriptor[K, V, (A, List[A])]](
      head.xmap((a: A) => (a, Nil), (b: (A, List[A])) => b._1)
    )(
      (b: ConfigDescriptor[K, V, (A, List[A])], a: ConfigDescriptor[K, V, A]) =>
        b.xmapEither2(a)(
          { case ((first, tail), a)    => Right((first, a :: tail)) }, {
            case (_, Nil)              => Left("Invalid list length")
            case (first, head :: tail) => Right(((first, tail), head))
          }
        )
    )

  val short: ConfigDescriptor[String, String, Short] =
    ConfigDescriptor.Source(ConfigSource.empty, PropertyType.ShortType) ?? "value of type short"

  def short(path: String): ConfigDescriptor[String, String, Short] = nested(path)(short)

  val string: ConfigDescriptor[String, String, String] =
    ConfigDescriptor.Source(ConfigSource.empty, PropertyType.StringType) ?? "value of type string"

  def string(path: String): ConfigDescriptor[String, String, String] = nested(path)(string)

  val uri: ConfigDescriptor[String, String, URI] =
    ConfigDescriptor.Source(ConfigSource.empty, PropertyType.UriType) ?? "value of type uri"

  def uri(path: String): ConfigDescriptor[String, String, URI] = nested(path)(uri)

  val uuid: ConfigDescriptor[String, String, UUID] =
    ConfigDescriptor.Source(ConfigSource.empty, PropertyType.UuidType) ?? "value of type uuid"

  def uuid(path: String): ConfigDescriptor[String, String, UUID] = nested(path)(uuid)

  val localDate: ConfigDescriptor[String, String, LocalDate] =
    ConfigDescriptor.Source(ConfigSource.empty, PropertyType.LocalDateType) ?? "value of type localdate"

  def localDate(path: String): ConfigDescriptor[String, String, LocalDate] = nested(path)(localDate)

  val localTime: ConfigDescriptor[String, String, LocalTime] =
    ConfigDescriptor.Source(ConfigSource.empty, PropertyType.LocalTimeType) ?? "value of type localtime"

  def localTime(path: String): ConfigDescriptor[String, String, LocalTime] = nested(path)(localTime)

  val localDateTime: ConfigDescriptor[String, String, LocalDateTime] =
    ConfigDescriptor.Source(ConfigSource.empty, PropertyType.LocalDateTimeType) ?? "value of type localdatetime"

  def localDateTime(path: String): ConfigDescriptor[String, String, LocalDateTime] = nested(path)(localDateTime)

  val instant: ConfigDescriptor[String, String, Instant] =
    ConfigDescriptor.Source(ConfigSource.empty, PropertyType.InstantType) ?? "value of type instant"

  def instant(path: String): ConfigDescriptor[String, String, Instant] = nested(path)(instant)

  val file: ConfigDescriptor[String, String, File] =
    ConfigDescriptor.Source(ConfigSource.empty, PropertyType.FileType) ?? "value of type file"

  def file(path: String): ConfigDescriptor[String, String, File] = nested(path)(file)

  val url: ConfigDescriptor[String, String, URL] =
    ConfigDescriptor.Source(ConfigSource.empty, PropertyType.UrlType) ?? "value of type URL"

  def url(path: String): ConfigDescriptor[String, String, URL] = nested(path)(url)

  def set[K, V, A](desc: ConfigDescriptor[K, V, A]): ConfigDescriptor[K, V, Set[A]] =
    list(desc).xmapEither(distinctListToSet, s => Right(s.toList))

  def set[K, V, A](path: K)(desc: ConfigDescriptor[K, V, A]): ConfigDescriptor[K, V, Set[A]] =
    nested(path)(set(desc))

  def setStrict[K, V, A](desc: ConfigDescriptor[K, V, A]): ConfigDescriptor[K, V, Set[A]] =
    listStrict(desc).xmapEither(distinctListToSet, s => Right(s.toList))

  def setStrict[K, V, A](path: K)(desc: ConfigDescriptor[K, V, A]): ConfigDescriptor[K, V, Set[A]] =
    nested(path)(setStrict(desc))

  private def distinctListToSet[A](list: List[A]): Either[String, Set[A]] =
    if (list.size == list.distinct.size) Right(list.toSet) else Left("Duplicated values found")
}
