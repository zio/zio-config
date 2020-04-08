package zio.config

import scala.concurrent.duration.Duration
import java.net.URI
import java.time.{ Instant, LocalDate, LocalDateTime, LocalTime }
import java.util.UUID

import zio.config.ConfigDescriptor._

sealed trait ConfigDescriptor[A] { self =>
  def apply[B](app: A => B, unapp: B => Option[A]): ConfigDescriptor[B] =
    XmapEither(
      this,
      (a: A) => Right[String, B](app(a)),
      unapp(_)
        .fold[Either[String, A]](Left("Unable to create case class instance"))(Right(_))
    )

  final def ??(description: String): ConfigDescriptor[A] =
    describe(description)

  final def |@|[B](f: => ConfigDescriptor[B]): ProductBuilder[A, B] =
    new ProductBuilder[A, B] {
      override val a: ConfigDescriptor[A] = self
      override val b: ConfigDescriptor[B] = f
    }

  final def <>(that: => ConfigDescriptor[A]): ConfigDescriptor[A] =
    self orElse that

  final def <*>[B](that: => ConfigDescriptor[B]): ConfigDescriptor[(A, B)] =
    self zip that

  final def <+>[B](that: => ConfigDescriptor[B]): ConfigDescriptor[Either[A, B]] =
    self orElseEither that

  final def default(value: A): ConfigDescriptor[A] =
    ConfigDescriptor.Default(self, value) ?? s"default value: $value"

  final def describe(description: String): ConfigDescriptor[A] =
    ConfigDescriptor.Describe(self, description)

  final def from(that: ConfigSource): ConfigDescriptor[A] =
    self.updateSource(_.orElse(that))

  def mapKey(f: String => String): ConfigDescriptor[A] = {
    def loop[B](config: ConfigDescriptor[B]): ConfigDescriptor[B] = config match {
      case Source(source, propertyType) => Source(source, propertyType)
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

  final def optional: ConfigDescriptor[Option[A]] =
    ConfigDescriptor.Optional(self) ?? "optional value"

  final def orElse(that: => ConfigDescriptor[A]): ConfigDescriptor[A] =
    ConfigDescriptor.OrElse(self, that)

  final def orElseEither[B](
    that: => ConfigDescriptor[B]
  ): ConfigDescriptor[Either[A, B]] =
    ConfigDescriptor.OrElseEither(self, that)

  final def unsourced: ConfigDescriptor[A] =
    self.updateSource(_ => ConfigSource.empty)

  final def updateSource(f: ConfigSource => ConfigSource): ConfigDescriptor[A] = {
    def loop[B](config: ConfigDescriptor[B]): ConfigDescriptor[B] = config match {
      case Source(source, propertyType) => Source(f(source), propertyType)
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

  final def xmap[B](to: A => B, from: B => A): ConfigDescriptor[B] =
    self.xmapEither(a => Right(to(a)), b => Right(from(b)))

  final def xmapEither[B](f: A => Either[String, B], g: B => Either[String, A]): ConfigDescriptor[B] =
    ConfigDescriptor.XmapEither(self, f, g)

  def xmapEither2[B, C](
    that: ConfigDescriptor[B]
  )(f: (A, B) => Either[String, C], g: C => Either[String, (A, B)]): ConfigDescriptor[C] =
    (self |@| that)
      .apply[(A, B)](Tuple2.apply, Tuple2.unapply)
      .xmapEither({ case (a, b) => f(a, b) }, g)

  final def zip[B](that: => ConfigDescriptor[B]): ConfigDescriptor[(A, B)] =
    ConfigDescriptor.Zip(self, that)
}

object ConfigDescriptor {
  final case class Default[A](config: ConfigDescriptor[A], value: A) extends ConfigDescriptor[A]

  final case class Describe[A](config: ConfigDescriptor[A], message: String) extends ConfigDescriptor[A]

  final case class Nested[A](path: String, config: ConfigDescriptor[A]) extends ConfigDescriptor[A]

  final case class Optional[A](config: ConfigDescriptor[A]) extends ConfigDescriptor[Option[A]]

  final case class OrElse[A](left: ConfigDescriptor[A], right: ConfigDescriptor[A]) extends ConfigDescriptor[A]

  final case class OrElseEither[A, B](left: ConfigDescriptor[A], right: ConfigDescriptor[B])
      extends ConfigDescriptor[Either[A, B]]

  final case class Sequence[A](source: ConfigSource, config: ConfigDescriptor[A]) extends ConfigDescriptor[List[A]]

  final case class Source[A](source: ConfigSource, propertyType: PropertyType[A]) extends ConfigDescriptor[A]

  final case class Zip[A, B](left: ConfigDescriptor[A], right: ConfigDescriptor[B]) extends ConfigDescriptor[(A, B)]

  final case class XmapEither[A, B](
    config: ConfigDescriptor[A],
    f: A => Either[String, B],
    g: B => Either[String, A]
  ) extends ConfigDescriptor[B]

  val bigDecimal: ConfigDescriptor[BigDecimal] =
    ConfigDescriptor.Source(ConfigSource.empty, PropertyType.BigDecimalType) ?? "value of type bigdecimal"

  def bigDecimal(path: String): ConfigDescriptor[BigDecimal] = nested(path)(bigDecimal)

  val bigInt: ConfigDescriptor[BigInt] =
    ConfigDescriptor.Source(ConfigSource.empty, PropertyType.BigIntType) ?? "value of type bigint"

  def bigInt(path: String): ConfigDescriptor[BigInt] = nested(path)(bigInt)

  val boolean: ConfigDescriptor[Boolean] =
    ConfigDescriptor.Source(ConfigSource.empty, PropertyType.BooleanType) ?? "value of type boolean"

  def boolean(path: String): ConfigDescriptor[Boolean] = nested(path)(boolean)

  val byte: ConfigDescriptor[Byte] =
    ConfigDescriptor.Source(ConfigSource.empty, PropertyType.ByteType) ?? "value of type byte"

  def byte(path: String): ConfigDescriptor[Byte] = nested(path)(byte)

  def collectAll[A](configList: ::[ConfigDescriptor[A]]): ConfigDescriptor[::[A]] =
    sequence(configList)

  val double: ConfigDescriptor[Double] =
    ConfigDescriptor.Source(ConfigSource.empty, PropertyType.DoubleType) ?? "value of type double"

  def double(path: String): ConfigDescriptor[Double] = nested(path)(double)

  val duration: ConfigDescriptor[Duration] =
    ConfigDescriptor.Source(ConfigSource.empty, PropertyType.DurationType) ?? "value of type duration"

  def duration(path: String): ConfigDescriptor[Duration] = nested(path)(duration)

  val zioDuration: ConfigDescriptor[zio.duration.Duration] =
    ConfigDescriptor.Source(ConfigSource.empty, PropertyType.ZioDurationType) ?? "value of type duration"

  def zioDuration(path: String): ConfigDescriptor[zio.duration.Duration] = nested(path)(zioDuration)

  val float: ConfigDescriptor[Float] =
    ConfigDescriptor.Source(ConfigSource.empty, PropertyType.FloatType) ?? "value of type float"

  def head[A](desc: ConfigDescriptor[A]): ConfigDescriptor[A] =
    desc.orElse(
      listStrict(desc)
        .xmapEither[A](_.headOption.fold[Either[String, A]](Left("Element is missing"))(Right(_)), v => Right(v :: Nil))
    )

  def head[A](path: String)(desc: ConfigDescriptor[A]): ConfigDescriptor[A] =
    nested(path)(head(desc))

  def float(path: String): ConfigDescriptor[Float] = nested(path)(float)

  val int: ConfigDescriptor[Int] =
    ConfigDescriptor.Source(ConfigSource.empty, PropertyType.IntType) ?? "value of type int"

  def int(path: String): ConfigDescriptor[Int] = nested(path)(int)

  /**
   * Allows scalar value instead of list
   * */
  def list[A](desc: ConfigDescriptor[A]): ConfigDescriptor[List[A]] =
    listStrict(desc).orElse(desc(_ :: Nil, _.headOption))

  /**
   * Allows scalar value instead of list
   * */
  def list[A](path: String)(desc: ConfigDescriptor[A]): ConfigDescriptor[List[A]] =
    nested(path)(list(desc))

  /**
   * Rejects scalar value in place of list
   * */
  def listStrict[A](desc: ConfigDescriptor[A]): ConfigDescriptor[List[A]] =
    ConfigDescriptor.Sequence(ConfigSource.empty, desc)

  /**
   * Rejects scalar value in place of list
   * */
  def listStrict[A](path: String)(desc: ConfigDescriptor[A]): ConfigDescriptor[List[A]] =
    nested(path)(listStrict(desc))

  val long: ConfigDescriptor[Long] =
    ConfigDescriptor.Source(ConfigSource.empty, PropertyType.LongType) ?? "value of type long"

  def long(path: String): ConfigDescriptor[Long] = nested(path)(long)

  def nested[A](path: String)(desc: ConfigDescriptor[A]): ConfigDescriptor[A] =
    ConfigDescriptor.Nested(path, desc)

  def sequence[A](configList: ::[ConfigDescriptor[A]]): ConfigDescriptor[::[A]] = {
    val reversed = configList.reverse
    reversed.tail.foldLeft[ConfigDescriptor[::[A]]](
      reversed.head.xmap((a: A) => ::(a, Nil), (b: ::[A]) => b.head)
    )(
      (b: ConfigDescriptor[::[A]], a: ConfigDescriptor[A]) =>
        b.xmapEither2(a)(
          (as: ::[A], a: A) => Right(::(a, as)),
          (t: ::[A]) => Right((::(t.tail.head, t.tail.tail), t.head))
        )
    )
  }

  val short: ConfigDescriptor[Short] =
    ConfigDescriptor.Source(ConfigSource.empty, PropertyType.ShortType) ?? "value of type short"

  def short(path: String): ConfigDescriptor[Short] = nested(path)(short)

  val string: ConfigDescriptor[String] =
    ConfigDescriptor.Source(ConfigSource.empty, PropertyType.StringType) ?? "value of type string"

  def string(path: String): ConfigDescriptor[String] = nested(path)(string)

  val uri: ConfigDescriptor[URI] =
    ConfigDescriptor.Source(ConfigSource.empty, PropertyType.UriType) ?? "value of type uri"

  def uri(path: String): ConfigDescriptor[URI] = nested(path)(uri)

  val uuid: ConfigDescriptor[UUID] =
    ConfigDescriptor.Source(ConfigSource.empty, PropertyType.UuidType) ?? "value of type uuid"

  def uuid(path: String): ConfigDescriptor[UUID] = nested(path)(uuid)

  val localDate: ConfigDescriptor[LocalDate] =
    ConfigDescriptor.Source(ConfigSource.empty, PropertyType.LocalDateType) ?? "value of type localdate"

  def localDate(path: String): ConfigDescriptor[LocalDate] = nested(path)(localDate)

  val localTime: ConfigDescriptor[LocalTime] =
    ConfigDescriptor.Source(ConfigSource.empty, PropertyType.LocalTimeType) ?? "value of type localtime"

  def localTime(path: String): ConfigDescriptor[LocalTime] = nested(path)(localTime)

  val localDateTime: ConfigDescriptor[LocalDateTime] =
    ConfigDescriptor.Source(ConfigSource.empty, PropertyType.LocalDateTimeType) ?? "value of type localdatetime"

  def localDateTime(path: String): ConfigDescriptor[LocalDateTime] = nested(path)(localDateTime)

  val instant: ConfigDescriptor[Instant] =
    ConfigDescriptor.Source(ConfigSource.empty, PropertyType.InstantType) ?? "value of type instant"

  def instant(path: String): ConfigDescriptor[Instant] = nested(path)(instant)
}
