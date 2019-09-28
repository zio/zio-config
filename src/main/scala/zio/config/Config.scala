package zio.config

abstract class Config[A] {
  self =>
  def zip[B](that: => Config[B]): Config[(A, B)] = Config.Zip(self, that)

  def mapEither[B](f: A => Either[ReadError, B])(g: B => Either[WriteError, A]): Config.ErrorXMap[A, B] =
    Config.ErrorXMap(self, f, g)

  def onError(f: => List[ReadError] => A): Config[A] = Config.OnError(self, f)

  def or[B](that: => Config[B]): Config[Either[A, B]] = Config.Or(self, that)

  def <+>[B](that: => Config[B]): Config[Either[A, B]] = self or that

  def xmap[B](to: A => B)(from: B => A): Config[B] = Config.Xmap(self, to, from)
}

object Config {

  final case class Source[A](path: String, propertyType: PropertyType[A]) extends Config[A]

  final case class ErrorXMap[A, B](config: Config[A], f: A => Either[ReadError, B], g: B => Either[WriteError, A])
      extends Config[B]

  final case class OnError[A](config: Config[A], f: List[ReadError] => A) extends Config[A]

  final case class Zip[A, B](left: Config[A], right: Config[B]) extends Config[(A, B)]

  final case class Xmap[A, B](config: Config[A], to: A => B, from: B => A) extends Config[B]

  final case class Or[A, B](left: Config[A], right: Config[B]) extends Config[Either[A, B]]

}
