package zio.config

import VersionSpecificSupport._

trait ConfigDescriptorModule extends ConfigSourceModule { module =>
  import ConfigDescriptorAdt._

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
      Default(self, value) ?? s"default value: $value"

    final def describe(description: String): ConfigDescriptor[A] =
      Describe(self, description)

    final def from(that: ConfigSource): ConfigDescriptor[A] =
      self.updateSource(_.orElse(that))

    def mapKey(f: K => K): ConfigDescriptor[A] = {
      def loop[B](config: ConfigDescriptor[B]): ConfigDescriptor[B] = config match {
        case Source(source, propertyType) => Source(source, propertyType)
        case DynamicMap(source, conf)     => DynamicMap(source, loop(conf))
        case Nested(path, conf)           => Nested(f(path), loop(conf))
        case Sequence(source, conf)       => Sequence(source, loop(conf))
        case Describe(config, message)    => Describe(loop(config), message)
        case Default(value, value2)       => Default(loop(value), value2)
        case XmapEither(config, f, g)     => XmapEither(loop(config), f, g)
        case Zip(conf1, conf2)            => Zip(loop(conf1), loop(conf2))
        case OrElseEither(value1, value2) => OrElseEither(loop(value1), loop(value2))
        case OrElse(value1, value2)       => OrElse(loop(value1), loop(value2))
      }

      loop(self)
    }

    final def optional: ConfigDescriptor[Option[A]] =
      Default(
        self.xmapEither[Option[A]](
          value => Right(Some(value)),
          option => option.fold[Either[String, A]](Left("Failed to write the non option"))(value => Right(value))
        ),
        None: Option[A]
      ) ?? "optional value"

    final def orElse(that: => ConfigDescriptor[A]): ConfigDescriptor[A] =
      OrElse(self, that)

    final def orElseEither[B](
      that: => ConfigDescriptor[B]
    ): ConfigDescriptor[Either[A, B]] =
      OrElseEither(self, that)

    final def requiredTerms: Int = {
      def loop[B](count: List[K], config: ConfigDescriptor[B]): Int =
        config match {
          case Zip(left, right)        => loop(count, left) + loop(count, right)
          case XmapEither(cfg, _, _)   => loop(count, cfg)
          case Describe(cfg, _)        => loop(count, cfg)
          case Nested(_, next)         => loop(count, next)
          case Source(_, _)            => 1
          case OrElse(cfg, cfg2)       => loop(count, cfg) + loop(count, cfg2)
          case OrElseEither(cfg, cfg2) => loop(count, cfg) + loop(count, cfg2)
          case Default(_, _)           => 0
          case Sequence(_, config)     => loop(count, config)
          case DynamicMap(_, config)   => loop(count, config)
        }

      loop(Nil, self)
    }

    final def unsourced: ConfigDescriptor[A] =
      self.updateSource(_ => ConfigSourceFunctions.empty)

    final def updateSource(f: ConfigSource => ConfigSource): ConfigDescriptor[A] = {
      def loop[B](config: ConfigDescriptor[B]): ConfigDescriptor[B] = config match {
        case Source(source, propertyType) => Source(f(source), propertyType)
        case DynamicMap(source, conf)     => DynamicMap(f(source), loop(conf))
        case Nested(path, conf)           => Nested(path, loop(conf))
        case Sequence(source, conf)       => Sequence(f(source), loop(conf))
        case Describe(config, message)    => Describe(loop(config), message)
        case Default(value, value2)       => Default(loop(value), value2)
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
      XmapEither(self, f, g)

    def xmapEither2[B, C](
      that: ConfigDescriptor[B]
    )(f: (A, B) => Either[String, C], g: C => Either[String, (A, B)]): ConfigDescriptor[C] =
      (self |@| that)
        .apply[(A, B)](Tuple2.apply, Tuple2.unapply)
        .xmapEither({ case (a, b) => f(a, b) }, g)

    def xmapEitherE[E, B](f: A => Either[E, B])(g: B => Either[E, A])(h: E => String): ConfigDescriptor[B] =
      self.xmapEither[B](
        (a: A) => ((f(a): Either[E, B]).swap: Either[B, E]).map(h).swap,
        (b: B) => ((g(b): Either[E, A]).swap: Either[A, E]).map(h).swap
      )

    def xmapEitherELeftPartial[E, B](f: A => Either[E, B])(g: B => A)(h: E => String): ConfigDescriptor[B] =
      self.xmapEitherE[E, B](f)(b => Right(g(b)))(h)

    def xmapEitherERightPartial[E, B](f: A => B)(g: B => Either[E, A])(h: E => String): ConfigDescriptor[B] =
      self.xmapEitherE[E, B](t => Right(f(t)))(g)(h)

    def xmapEitherLeftPartial[B](f: A => Either[String, B], g: B => A): ConfigDescriptor[B] =
      xmapEitherELeftPartial(f)(g)(identity)

    def xmapEitherRightPartial[B](f: A => B, g: B => Either[String, A]): ConfigDescriptor[B] =
      xmapEitherERightPartial(f)(g)(identity)

    final def zip[B](that: => ConfigDescriptor[B]): ConfigDescriptor[(A, B)] =
      Zip(self, that)
  }

  trait ConfigDescriptorFunctions {

    def collectAll[A](
      head: ConfigDescriptor[A],
      tail: ConfigDescriptor[A]*
    ): ConfigDescriptor[List[A]] =
      sequence(head, tail: _*)({ case (a, t) => a :: t }, l => l.headOption.map(h => (h, l.tail)))

    def head[A](desc: ConfigDescriptor[A]): ConfigDescriptor[A] =
      desc.orElse(
        listStrict(desc)
          .xmapEither[A](
            _.headOption.fold[Either[String, A]](Left("Element is missing"))(Right(_)),
            v => Right(v :: Nil)
          )
      )

    def head[A](path: K)(desc: ConfigDescriptor[A]): ConfigDescriptor[A] =
      nested(path)(head(desc))

    def list[K, V, A](desc: ConfigDescriptor[A]): ConfigDescriptor[List[A]] =
      listStrict(desc)

    def list[A](path: K)(desc: ConfigDescriptor[A]): ConfigDescriptor[List[A]] =
      nested(path)(list(desc))

    def listFlexible[A](path: K)(desc: ConfigDescriptor[A]): ConfigDescriptor[List[A]] =
      listStrict(path)(desc) orElse (desc.xmapEither[List[A]](value => Right(List(value)), _.headOption match {
        case Some(value) => Right(value)
        case None        => Left("Cannot write an empty list back")
      }))

    def listStrict[A](desc: ConfigDescriptor[A]): ConfigDescriptor[List[A]] =
      Sequence(ConfigSourceFunctions.empty, desc)

    def listStrict[A](path: K)(desc: ConfigDescriptor[A]): ConfigDescriptor[List[A]] =
      nested(path)(listStrict(desc))

    def map[A](desc: ConfigDescriptor[A]): ConfigDescriptor[Map[K, A]] =
      mapStrict[A](desc)

    def map[A](path: K)(desc: ConfigDescriptor[A]): ConfigDescriptor[Map[K, A]] =
      nested(path)(mapStrict(desc))

    def mapStrict[A](desc: ConfigDescriptor[A]): ConfigDescriptor[Map[K, A]] =
      DynamicMap(ConfigSourceFunctions.empty, desc)

    def nested[A](path: K)(desc: ConfigDescriptor[A]): ConfigDescriptor[A] =
      Nested(path, desc)

    def sequence[A](
      head: ConfigDescriptor[A],
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

    def set[K, V, A](desc: ConfigDescriptor[A]): ConfigDescriptor[Set[A]] =
      list(desc).xmapEither(distinctListToSet, s => Right(s.toList))

    def set[A](path: K)(desc: ConfigDescriptor[A]): ConfigDescriptor[Set[A]] =
      nested(path)(set(desc))

    def setStrict[A](desc: ConfigDescriptor[A]): ConfigDescriptor[Set[A]] =
      listStrict(desc).xmapEither(distinctListToSet, s => Right(s.toList))

    def setStrict[A](path: K)(desc: ConfigDescriptor[A]): ConfigDescriptor[Set[A]] =
      nested(path)(setStrict(desc))

    private def distinctListToSet[A](list: List[A]): Either[String, Set[A]] =
      if (list.size == list.distinct.size) Right(list.toSet) else Left("Duplicated values found")

  }

  object ConfigDescriptorAdt {
    case class Default[A](config: ConfigDescriptor[A], default: A) extends ConfigDescriptor[A]

    case class Describe[A](config: ConfigDescriptor[A], message: String) extends ConfigDescriptor[A]

    case class DynamicMap[A](source: ConfigSource, config: ConfigDescriptor[A]) extends ConfigDescriptor[Map[K, A]]

    case class Nested[A](path: K, config: ConfigDescriptor[A]) extends ConfigDescriptor[A]

    case class OrElse[A](left: ConfigDescriptor[A], right: ConfigDescriptor[A]) extends ConfigDescriptor[A]

    case class OrElseEither[A, B](left: ConfigDescriptor[A], right: ConfigDescriptor[B])
        extends ConfigDescriptor[Either[A, B]]

    case class Sequence[A](source: ConfigSource, config: ConfigDescriptor[A]) extends ConfigDescriptor[List[A]]

    case class Source[A](source: ConfigSource, propertyType: PropertyType[V, A]) extends ConfigDescriptor[A]

    case class Zip[A, B](left: ConfigDescriptor[A], right: ConfigDescriptor[B]) extends ConfigDescriptor[(A, B)]

    case class XmapEither[A, B](config: ConfigDescriptor[A], f: A => Either[String, B], g: B => Either[String, A])
        extends ConfigDescriptor[B]
  }

}
