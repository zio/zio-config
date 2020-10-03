package zio.config

import VersionSpecificSupport._

trait ConfigDescriptorModule extends ConfigSourceModule { module =>
  import ConfigDescriptorAdt._

  sealed trait ConfigDescriptor[A] { self =>
    def apply[B](app: A => B, unapp: B => Option[A]): ConfigDescriptor[B] =
      XmapEither(
        thunk(this),
        (a: A) => Right[String, B](app(a)),
        unapp(_)
          .fold[Either[String, A]](Left("Unable to create case class instance"))(Right(_))
      )

    final def ??(description: String): ConfigDescriptor[A] =
      describe(description)

    final def |@|[B](that: => ConfigDescriptor[B]): ProductBuilder[ConfigDescriptor, A, B] =
      new ProductBuilder[ConfigDescriptor, A, B] {

        override def zip[X, Y]: (ConfigDescriptor[X], ConfigDescriptor[Y]) => ConfigDescriptor[(X, Y)] =
          (a, b) => a.zip(b)

        override def xmapEither[X, Y]
          : (ConfigDescriptor[X], X => Either[String, Y], Y => Either[String, X]) => ConfigDescriptor[Y] =
          (a, b, c) => a.xmapEither(b, c)

        override val a: ConfigDescriptor[A] = self
        override val b: ConfigDescriptor[B] = that
      }

    final def <>(that: => ConfigDescriptor[A]): ConfigDescriptor[A] =
      self orElse that

    final def <*>[B](that: => ConfigDescriptor[B]): ConfigDescriptor[(A, B)] =
      self zip that

    final def <+>[B](that: => ConfigDescriptor[B]): ConfigDescriptor[Either[A, B]] =
      self orElseEither that

    final def default(value: A): ConfigDescriptor[A] =
      Default(thunk(self), value) ?? s"default value: $value"

    final def describe(description: String): ConfigDescriptor[A] =
      Describe(thunk(self), description)

    final def from(that: ConfigSource): ConfigDescriptor[A] =
      self.updateSource(_.orElse(that))

    def mapKey(f: K => K): ConfigDescriptor[A] = {
      def loop[B](config: ConfigDescriptor[B]): ConfigDescriptor[B] = config match {
        case Source(source, propertyType) => Source(source, propertyType)
        case DynamicMap(source, conf)     => DynamicMap(source, conf.map(loop))
        case Nested(source, path, conf)   => Nested(source, f(path), conf.map(loop))
        case Optional(conf)               => Optional(conf.map(loop))
        case Sequence(source, conf)       => Sequence(source, conf.map(loop))
        case Describe(conf, message)      => Describe(conf.map(loop), message)
        case Default(value, value2)       => Default(value.map(loop), value2)
        case XmapEither(config, f, g)     => XmapEither(config.map(loop), f, g)
        case Zip(conf1, conf2)            => Zip(conf1.map(loop), conf2.map(loop))
        case OrElseEither(value1, value2) => OrElseEither(value1.map(loop), value2.map(loop))
        case OrElse(value1, value2)       => OrElse(value1.map(loop), value2.map(loop))
      }

      loop(self)
    }

    final def optional: ConfigDescriptor[Option[A]] =
      Optional(thunk(self)) ?? "optional value"

    final def orElse(that: => ConfigDescriptor[A]): ConfigDescriptor[A] =
      OrElse(thunk(self), thunk(that))

    final def orElseEither[B](
      that: => ConfigDescriptor[B]
    ): ConfigDescriptor[Either[A, B]] =
      OrElseEither(thunk(self), thunk(that))

    final def unsourced: ConfigDescriptor[A] =
      self.updateSource(_ => ConfigSourceFunctions.empty)

    final def updateSource(f: ConfigSource => ConfigSource): ConfigDescriptor[A] = {
      def loop[B](config: ConfigDescriptor[B]): ConfigDescriptor[B] = config match {
        case Source(source, propertyType) => Source(f(source), propertyType)
        case DynamicMap(source, conf)     => DynamicMap(f(source), conf.map(loop))
        case Nested(source, path, conf)   => Nested(f(source), path, conf.map(loop))
        case Optional(conf)               => Optional(conf.map(loop))
        case Sequence(source, conf)       => Sequence(f(source), conf.map(loop))
        case Describe(conf, message)      => Describe(conf.map(loop), message)
        case Default(value, value2)       => Default(value.map(loop), value2)
        case XmapEither(conf, f, g)       => XmapEither(conf.map(loop), f, g)
        case Zip(conf1, conf2)            => Zip(conf1.map(loop), conf2.map(loop))
        case OrElseEither(value1, value2) => OrElseEither(value1.map(loop), value2.map(loop))
        case OrElse(value1, value2)       => OrElse(value1.map(loop), value2.map(loop))
      }

      loop(self)
    }

    final def transform[B](to: A => B, from: B => A): ConfigDescriptor[B] =
      self.xmapEither(a => Right(to(a)), b => Right(from(b)))

    final def transformEither[B](f: A => Either[String, B], g: B => Either[String, A]): ConfigDescriptor[B] =
      self.xmapEither(f, g)

    final def transformEitherLeft[B](f: A => Either[String, B], g: B => A): ConfigDescriptor[B] =
      self.transformEitherLeft(f)(g)(identity)

    final def transformEitherLeft[E, B](f: A => Either[E, B])(g: B => A)(h: E => String): ConfigDescriptor[B] =
      self.xmapEither[E, B](f)(b => Right(g(b)))(h)

    final def transformEitherRight[B](f: A => B, g: B => Either[String, A]): ConfigDescriptor[B] =
      self.transformEitherRight(f)(g)(identity)

    final def transformEitherRight[E, B](f: A => B)(g: B => Either[E, A])(h: E => String): ConfigDescriptor[B] =
      self.xmapEither[E, B](t => Right(f(t)))(g)(h)

    final def xmap[B](to: A => B, from: B => A): ConfigDescriptor[B] =
      self.xmapEither(a => Right(to(a)), b => Right(from(b)))

    final def xmapEither[B](f: A => Either[String, B], g: B => Either[String, A]): ConfigDescriptor[B] =
      XmapEither(thunk(self), f, g)

    final def xmapEither[E, B](f: A => Either[E, B])(g: B => Either[E, A])(h: E => String): ConfigDescriptor[B] =
      self.xmapEither[B](
        (a: A) => ((f(a): Either[E, B]).swap: Either[B, E]).map(h).swap,
        (b: B) => ((g(b): Either[E, A]).swap: Either[A, E]).map(h).swap
      )

    final def xmapEither2[B, C](
      that: => ConfigDescriptor[B]
    )(f: (A, B) => Either[String, C], g: C => Either[String, (A, B)]): ConfigDescriptor[C] =
      (self |@| that)
        .apply[(A, B)](Tuple2.apply, Tuple2.unapply)
        .xmapEither({ case (a, b) => f(a, b) }, g)

    final def zip[B](that: => ConfigDescriptor[B]): ConfigDescriptor[(A, B)] =
      Zip(thunk(self), thunk(that))
  }

  trait ConfigDescriptorFunctions {

    def collectAll[A](
      head: ConfigDescriptor[A],
      tail: ConfigDescriptor[A]*
    ): ConfigDescriptor[List[A]] =
      sequence(head, tail: _*)({ case (a, t) => a :: t }, l => l.headOption.map(h => (h, l.tail)))

    def head[A](desc: ConfigDescriptor[A]): ConfigDescriptor[A] =
      desc.orElse(
        list(desc)
          .xmapEither[A](
            _.headOption.fold[Either[String, A]](Left("Element is missing"))(Right(_)),
            v => Right(v :: Nil)
          )
      )

    def head[A](path: K)(desc: => ConfigDescriptor[A]): ConfigDescriptor[A] =
      nested(path)(head(desc))

    def list[K, V, A](desc: => ConfigDescriptor[A]): ConfigDescriptor[List[A]] =
      Sequence(ConfigSourceFunctions.empty, thunk(desc))

    def list[A](path: K)(desc: => ConfigDescriptor[A]): ConfigDescriptor[List[A]] =
      nested(path)(list(desc))

    def listOrSingleton[A](path: K)(desc: => ConfigDescriptor[A]): ConfigDescriptor[List[A]] =
      list(path)(desc) orElse (
        desc.xmapEither[List[A]](
          value => Right(List(value)),
          _.headOption match {
            case Some(value) => Right(value)
            case None        => Left("Cannot write an empty list back")
          }
        )
      )

    def map[A](desc: => ConfigDescriptor[A]): ConfigDescriptor[Map[K, A]] =
      DynamicMap(ConfigSourceFunctions.empty, thunk(desc))

    def map[A](path: K)(desc: => ConfigDescriptor[A]): ConfigDescriptor[Map[K, A]] =
      nested(path)(map(desc))

    def nested[A](path: K)(desc: => ConfigDescriptor[A]): ConfigDescriptor[A] =
      Nested(ConfigSourceFunctions.empty, path, thunk(desc))

    def sequence[A](
      head: => ConfigDescriptor[A],
      tail: ConfigDescriptor[A]*
    ): ConfigDescriptor[(A, List[A])] =
      tail.reverse.foldLeft[ConfigDescriptor[(A, List[A])]](
        head.xmap((a: A) => (a, Nil), (b: (A, List[A])) => b._1)
      )(
        (b: ConfigDescriptor[(A, List[A])], a: ConfigDescriptor[A]) =>
          b.xmapEither2(a)(
            { case ((first, tail), a)    => Right((first, a :: tail)) }, {
              case (_, Nil)              => Left("Invalid list length")
              case (first, head :: tail) => Right(((first, tail), head))
            }
          )
      )

    def set[K, V, A](desc: => ConfigDescriptor[A]): ConfigDescriptor[Set[A]] =
      list(desc).xmapEither(distinctListToSet, s => Right(s.toList))

    def set[A](path: K)(desc: => ConfigDescriptor[A]): ConfigDescriptor[Set[A]] =
      nested(path)(set(desc))

    private def distinctListToSet[A](list: List[A]): Either[String, Set[A]] =
      if (list.size == list.distinct.size) Right(list.toSet) else Left("Duplicated values found")

  }

  case class LazyConfigDescriptor[A](get: () => ConfigDescriptor[A]) {
    def map[B](f: ConfigDescriptor[A] => ConfigDescriptor[B]): LazyConfigDescriptor[B] =
      LazyConfigDescriptor(() => f(get()))
  }

  private final def thunk[A](config: => ConfigDescriptor[A]): LazyConfigDescriptor[A] =
    LazyConfigDescriptor(() => config)

  object ConfigDescriptorAdt {
    case class Default[A](config: LazyConfigDescriptor[A], default: A) extends ConfigDescriptor[A]

    case class Describe[A](config: LazyConfigDescriptor[A], message: String) extends ConfigDescriptor[A]

    case class DynamicMap[A](source: ConfigSource, config: LazyConfigDescriptor[A]) extends ConfigDescriptor[Map[K, A]]

    case class Nested[A](source: ConfigSource, path: K, config: LazyConfigDescriptor[A]) extends ConfigDescriptor[A]

    case class Optional[A](config: LazyConfigDescriptor[A]) extends ConfigDescriptor[Option[A]]

    case class OrElse[A](left: LazyConfigDescriptor[A], right: LazyConfigDescriptor[A]) extends ConfigDescriptor[A]

    case class OrElseEither[A, B](left: LazyConfigDescriptor[A], right: LazyConfigDescriptor[B])
        extends ConfigDescriptor[Either[A, B]]

    case class Sequence[A](source: ConfigSource, config: LazyConfigDescriptor[A]) extends ConfigDescriptor[List[A]]

    case class Source[A](source: ConfigSource, propertyType: PropertyType[V, A]) extends ConfigDescriptor[A]

    case class Zip[A, B](left: LazyConfigDescriptor[A], right: LazyConfigDescriptor[B]) extends ConfigDescriptor[(A, B)]

    case class XmapEither[A, B](config: LazyConfigDescriptor[A], f: A => Either[String, B], g: B => Either[String, A])
        extends ConfigDescriptor[B]
  }

}
