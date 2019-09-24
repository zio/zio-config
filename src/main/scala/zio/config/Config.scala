package zio.config

abstract class Config[A] { self =>
  def zip[B](that: Config[B]): Config[(A, B)] = Config.Zip(self, that)

  def map[B](f: A => B): Config[B] = Config.Map(self, f)

  def errorMap[B](f: A => Either[ConfigError, B]) = Config.ErrorMap(self, f)

  def bimap[B](f: List[ConfigError] => B, g: A => B): Config[B] = Config.OnError(self, f, g)

  def orElseEither[B](that: Config[B]): Config[Either[A, B]] = Config.OrElseEither(self, that)

  def or[B](that: Config[B]): Config[Either[A, B]] = Config.OrElseEither(self, that)

  def <+>[B](that: Config[B]): Config[Either[A, B]] = self orElseEither that

  def |(that: Config[A]): Config[A] =
    (self <+> that).xmap({
      case Right(value) => value
      case Left(value)  => value
    }, value => Right(value))

  def xmap[B](to: A => B, from: B => A): Config[B] = Config.Xmap(self, to, from)
}

object Config {

  final case class Source[A](path: String, propertyType: PropertyType[A])                 extends Config[A]
  final case class ErrorMap[A, B](config: Config[A], f: A => Either[ConfigError, B])      extends Config[B]
  final case class Map[A, B](config: Config[A], f: A => B)                                extends Config[B]
  final case class FlatMap[A, B](config: Config[A], f: A => Config[B])                    extends Config[B]
  final case class OnError[A, B](config: Config[A], f: List[ConfigError] => B, g: A => B) extends Config[B]
  final case class Sources[A](propertyType: PropertyType[A], paths: Seq[String])          extends Config[A]
  final case class Zip[A, B](left: Config[A], right: Config[B])                           extends Config[(A, B)]
  final case class Xmap[A, B](config: Config[A], to: A => B, from: B => A)                extends Config[B]
  final case class OrElseEither[A, B](left: Config[A], right: Config[B])                  extends Config[Either[A, B]]

}
