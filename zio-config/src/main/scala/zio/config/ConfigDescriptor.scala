package zio.config

import zio.config.ReadErrors.ReadError

sealed trait ConfigDescriptor[A] { self =>
  final def zip[B](that: => ConfigDescriptor[B]): ConfigDescriptor[(A, B)] =
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
}

object ConfigDescriptor {

  final case class Empty[A]() extends ConfigDescriptor[Option[A]]

  final case class Source[A](path: String, propertyType: PropertyType[A]) extends ConfigDescriptor[A]

  final case class Nested[A](config: ConfigDescriptor[A], path: String) extends ConfigDescriptor[A]

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
}
