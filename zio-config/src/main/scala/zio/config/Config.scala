package zio.config

import java.net.URI

sealed trait Config[A] {
  self =>
  final def zip[B](that: => Config[B]): Config[(A, B)] = Config.Zip(self, that)

  final def xmapEither[B](f: A => Either[ReadError, B])(g: B => Either[String, A]): Config.MapEither[A, B] =
    Config.MapEither(self, f, g)

  final def onError(f: => ReadErrors => A): Config[A] = Config.OnError(self, f)

  final def or[B](that: => Config[B]): Config[Either[A, B]] = Config.Or(self, that)

  final def <+>[B](that: => Config[B]): Config[Either[A, B]] = self or that

  final def xmap[B](to: A => B)(from: B => A): Config[B] =
    self.xmapEither(a => Right(to(a)))(b => Right(from(b)))

  def xmapEither2[B, C](that: Config[B])(f: (A, B) => Either[ReadError, C])(g: C => Either[String, (A, B)]): Config[C] =
    (self |@| that).apply[(A, B)]((a, b) => (a, b), t => Some((t._1, t._2))).xmapEither(b => f(b._1, b._2))(g)

  final def |@|[B](f: => Config[B]): ProductBuilder[A, B] =
    new ProductBuilder[A, B] {
      override val a: Config[A] = self
      override val b: Config[B] = f
    }

  def optional: Config[Option[A]] =
    Config.Optional(self)
}

object Config {

  final case class Pure[A](a: A) extends Config[A]

  final case class Source[A](path: String, propertyType: PropertyType[A]) extends Config[A]

  final case class Optional[A](config: Config[A]) extends Config[Option[A]]

  final case class MapEither[A, B](config: Config[A], f: A => Either[ReadError, B], g: B => Either[String, A])
      extends Config[B]

  final case class OnError[A](config: Config[A], f: ReadErrors => A) extends Config[A]

  final case class Zip[A, B](left: Config[A], right: Config[B]) extends Config[(A, B)]

  final case class Or[A, B](left: Config[A], right: Config[B]) extends Config[Either[A, B]]

  def succeed[A](a: A): Config[A] =
    Config.Pure(a)

  def sequence[A](configList: List[Config[A]]): Config[List[A]] =
    configList.foldLeft(Pure(Nil): Config[List[A]])(
      (a, b) =>
        b.xmapEither2(a)((aa, bb) => Right(aa :: bb))(t => {
          t.headOption.fold[Either[String, (A, List[A])]](
            Left(
              "The input is not corresponding to the config description. It may have less number of entries than required by the config."
            )
          )(ll => Right((ll, t.tail)))
        })
    )

  def collectAll[A](configList: List[Config[A]]): Config[List[A]] =
    sequence(configList)

  def int(path: String): Config[Int]               = Config.Source(path, PropertyType.IntType)
  def double(path: String): Config[Double]         = Config.Source(path, PropertyType.DoubleType)
  def string(path: String): Config[String]         = Config.Source(path, PropertyType.StringType)
  def long(path: String): Config[Long]             = Config.Source(path, PropertyType.LongType)
  def short(path: String): Config[Short]           = Config.Source(path, PropertyType.ShortType)
  def uri(path: String): Config[URI]               = Config.Source(path, PropertyType.UriType)
  def bigDecimal(path: String): Config[BigDecimal] = Config.Source(path, PropertyType.BigDecimalType)
  def float(path: String): Config[Float]           = Config.Source(path, PropertyType.FloatType)
  def bigInt(path: String): Config[BigInt]         = Config.Source(path, PropertyType.BigIntType)
  def boolean(path: String): Config[Boolean]       = Config.Source(path, PropertyType.BooleanType)
  def byte(path: String): Config[Byte]             = Config.Source(path, PropertyType.ByteType)
}
