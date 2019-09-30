package zio.config

sealed trait Config[A] {
  self =>
  def zip[B](that: => Config[B]): Config[(A, B)] = Config.Zip(self, that)

  def mapEither[B](f: A => Either[ReadError, B])(g: B => Either[WriteError, A]): Config.MapEither[A, B] =
    Config.MapEither(self, f, g)

  def onError(f: => List[ReadError] => A): Config[A] = Config.OnError(self, f)

  def or[B](that: => Config[B]): Config[Either[A, B]] = Config.Or(self, that)

  def <+>[B](that: => Config[B]): Config[Either[A, B]] = self or that

  def xmap[B](to: A => B)(from: B => A): Config[B] =
    self.mapEither(a => Right(to(a)))(b => Right(from(b)))

  final def |@|[B](f: => Config[B]): ProductBuilder[A, B] =
    new ProductBuilder[A, B] {
      override val a: Config[A] = self
      override val b: Config[B] = f
    }

  def optional: Config[Option[A]] =
    Config.Optional(self)
}

object Config {

  final case class Source[A](path: String, propertyType: PropertyType[A]) extends Config[A]

  final case class Optional[A](config: Config[A]) extends Config[Option[A]]

  final case class MapEither[A, B](config: Config[A], f: A => Either[ReadError, B], g: B => Either[WriteError, A])
      extends Config[B]

  final case class OnError[A](config: Config[A], f: List[ReadError] => A) extends Config[A]

  final case class Zip[A, B](left: Config[A], right: Config[B]) extends Config[(A, B)]

  final case class Or[A, B](left: Config[A], right: Config[B]) extends Config[Either[A, B]]

}
