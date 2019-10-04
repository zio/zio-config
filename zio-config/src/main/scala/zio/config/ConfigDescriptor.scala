package zio.config

sealed trait ConfigDescriptor[A] {
  self =>
  final def zip[B](that: => ConfigDescriptor[B]): ConfigDescriptor[(A, B)] = ConfigDescriptor.Zip(self, that)
  final def <*>[B](that: => ConfigDescriptor[B]): ConfigDescriptor[(A, B)] = zip(that)

  final def xmapEither[B](f: A => Either[ReadError, B])(g: B => Either[String, A]): ConfigDescriptor.MapEither[A, B] =
    ConfigDescriptor.MapEither(self, f, g)

  final def or[B](that: => ConfigDescriptor[B]): ConfigDescriptor[Either[A, B]] = ConfigDescriptor.Or(self, that)

  final def <+>[B](that: => ConfigDescriptor[B]): ConfigDescriptor[Either[A, B]] = self or that

  def |(that: => ConfigDescriptor[A]): ConfigDescriptor[A] =
    (self or that).xmap {
      case Right(value) => value
      case Left(value)  => value
    }(b => Right(b))

  final def xmap[B](to: A => B)(from: B => A): ConfigDescriptor[B] =
    self.xmapEither(a => Right(to(a)))(b => Right(from(b)))

  def xmapEither2[B, C](
    that: ConfigDescriptor[B]
  )(f: (A, B) => Either[ReadError, C])(g: C => Either[String, (A, B)]): ConfigDescriptor[C] =
    (self |@| that).apply[(A, B)]((a, b) => (a, b), t => Some((t._1, t._2))).xmapEither(b => f(b._1, b._2))(g)

  final def |@|[B](f: => ConfigDescriptor[B]): ProductBuilder[A, B] =
    new ProductBuilder[A, B] {
      override val a: ConfigDescriptor[A] = self
      override val b: ConfigDescriptor[B] = f
    }

  def optional: ConfigDescriptor[Option[A]] =
    ConfigDescriptor.Optional(self) ~ "Optional value"

  def describe(description: String): ConfigDescriptor[A] =
    ConfigDescriptor.Describe(self, description)

  def default(value: A): ConfigDescriptor[A] =
    ConfigDescriptor.Default(self, value) ~ s"Default value: $value"

  def ~(description: String): ConfigDescriptor[A] =
    describe(description)
}

object ConfigDescriptor {

  final case class Succeed[A](a: A) extends ConfigDescriptor[A]

  final case class Source[A](path: String, propertyType: PropertyType[A]) extends ConfigDescriptor[A]

  final case class Describe[A](config: ConfigDescriptor[A], message: String) extends ConfigDescriptor[A]

  final case class Default[A](configDescriptor: ConfigDescriptor[A], value: A) extends ConfigDescriptor[A]

  final case class Optional[A](config: ConfigDescriptor[A]) extends ConfigDescriptor[Option[A]]

  final case class MapEither[A, B](config: ConfigDescriptor[A], f: A => Either[ReadError, B], g: B => Either[String, A])
      extends ConfigDescriptor[B]

  final case class OnError[A](config: ConfigDescriptor[A], f: ReadErrors => A) extends ConfigDescriptor[A]

  final case class Zip[A, B](left: ConfigDescriptor[A], right: ConfigDescriptor[B]) extends ConfigDescriptor[(A, B)]

  final case class Or[A, B](left: ConfigDescriptor[A], right: ConfigDescriptor[B])
      extends ConfigDescriptor[Either[A, B]]

  def sequence[A](configList: List[ConfigDescriptor[A]]): ConfigDescriptor[List[A]] =
    configList.foldLeft(Succeed(Nil): ConfigDescriptor[List[A]])(
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
