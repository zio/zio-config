package zio.config

import VersionSpecificSupport._

trait ConfigDescriptorModule extends ConfigSourceModule { module =>
  import ConfigDescriptorAdt._

  sealed trait ConfigDescriptor[A] { self =>

    /**
     * Given `A` and `B` the below `apply` function is used to convert a `ConfigDescriptor[A]` to `ConfigDescriptor[B]`.
     *
     * It is important to note that while we can retrieve B directly from A,
     * there is no guarantee that reverse relationship exists. i.e, B => A.
     * Instead the reverse relationship is `B => Option[A]`.
     * This is why the arguments of this function are `app: A => B` and `unapp: B => Option[A]`
     *
     * Why ?
     *
     * Let's define a simple `ConfigDescriptor`, that talks about retrieving a `String` configuration (example: PORT).
     *
     *  {{{
     *    val port: ConfigDescriptor[Int] = int("PORT")
     *     // NOTE: We are not attaching a real source here, because a config descriptor can exist without a source
     *  }}}
     *
     * Later you decided to convert the type of `Port` from `Int` to `String`
     * for a variety of reasons in your application.
     *
     * In this case. In this case, `A => B` is `Int => String` which will always work.
     *
     * However, the reverse relationship (which is used to retrieve
     * the original type from its transformed representation) is `String => Int`, which
     * is not a total function. i.e, Not all elements of set `String` can be converted to `Int`.
     * Example: "Australia" cannot be converted to `Int`.
     *
     * Hence we can do `s => Try(s.toInt).toOption` to mark the possibility of errors.
     * This is a function of the type: `B => Option[A]`.
     *
     * Also note that, you can make use of `transformEither` instead of `apply` which is a much more powerful method to
     * convert from one type to the other keeping the error information without falling back to `Option`.
     *
     *  {{{
     *
     *    val portString: ConfigDescriptor[Int] =
     *       port.apply[String](_.toString, (s: String) => Try(s.toInt).toOption)
     *
     *  }}}
     *
     *   With the above information you can both read a port from a source and convert to a string,
     *   and write the stringified port back to a source representation.
     *
     *  READ:
     *
     *  {{{
     *
     *    import zio.config._
     *    val configSource: ConfigSource = ConfigSource.fromMap(Map.empty)
     *
     *    // Read
     *    val configResult: Either[ReadError[String], String] = read(portString from configSource)
     *
     *  }}}
     *
     *  WRITE:
     *
     *  Now given a stringified port "8888", you can also write it back to the source safely (i.e, guaranteed that the read will work) because
     *  the it runs through `(s: String) => Try(s.toInt).toOption` and verify it is a real port that can be converted to `Int`.
     *
     *  {{{
     *
     *     import zio.config.typesafe._ // as toJson is available only through zio-config-typesafe module
     *
     *     val writtenBack: Either[String, PropertyTree[String, String]] = write(portString, "8888")
     *
     *     val jsonRepr: Either[String, String] = writtenBack.map(_.toJson) // { "port" : "8888" }
     *     val mapRepr: Either[String, Map[String, String]] = writtenBack.map(_.flattenString()) // Map("port" -> "8888")
     *  }}}
     */
    def apply[B](app: A => B, unapp: B => Option[A]): ConfigDescriptor[B] =
      XmapEither(
        thunk(this),
        (a: A) => Right[String, B](app(a)),
        unapp(_)
          .fold[Either[String, A]](
            Left("Unable to create case class instance")
          )(Right(_))
      )

    /**
     * `??` is an alias to `describe` which allows us to inject additional documentation to the configuration parameters.
     *
     * Example:
     *
     *  {{{ val port = int("PORT") ?? "database port" }}}
     *
     * A more detailed example:
     *
     * Here is a program that describes (or a ConfigDescriptor that represents) reading a `USERNAME` which is a String and `PORT` which is an Int,
     * and load it to a case class `Config`
     *
     *  {{{
     *     final case class Config(userName: String, port: Int)
     *
     *     object Config {
     *        val dbConfig: ConfigDescriptor[Config] =
     *           (string("USERNAME") |@| int("PORT"))(Config.apply, Config.unapply)
     *     }
     *  }}}
     *
     *   Later on you decided to annotate each one of them with extra documentation, which is later seen in error messages if config retrieval
     *   is a failure, and it's also used while documenting your configuration using `ConfigDocsModule`
     *
     *  {{{
     *    val dbConfigWithDoc: ConfigDescriptor[Config] =
     *       (string("USERNAME") ?? "db username" |@| int("PORT") ?? "db port")(Config.apply, Config.unapply)
     *  }}}
     *
     *  If you try and read this config from an empty source, it emits an error message with the details you provided.
     *
     *   {{{
     *     import zio.config._, ConfigDescriptor._
     *
     *     println(
     *        read(Config.databaseConfig from ConfigSource.fromMap(Map.empty))
     *      )
     *   }}}
     *
     *  returns:
     *
     *  {{{
     *   ╥
     *   ╠══╦══╗
     *   ║  ║  ║
     *   ║  ║  ╠─MissingValue
     *   ║  ║  ║ path: PORT
     *   ║  ║  ║ Details: db port, value of type int
     *   ║  ║  ▼
     *   ║  ║
     *   ║  ╠─MissingValue
     *   ║  ║ path: USERNAME
     *   ║  ║ Details: db username, value of type string
     *   ║  ▼
     *   ▼
     *
     *  }}}
     *
     *  Or, you can also use a common documentation for an entire set of config parameters.
     *
     *  {{{
     *    val detailedConfigDescriptor: ConfigDescriptor[Config] =
     *      configDescriptor ?? "Configuration related to database"
     *  }}}
     *
     */
    final def ??(description: String): ConfigDescriptor[A] =
      describe(description)

    /**
     * |@| is a ConfigDescriptor builder. We know `ConfigDescriptor` is a program that describes the retrieval of a set of configuration
     *  parameters.
     *
     *  Below given is a `ConfigDescriptor` that describes the retrieval of a single config.
     *
     *  {{{
     *    val port: ConfigDescriptor[String] = string("PORT")
     *  }}}
     *
     *  However, in order to retrieve multiple configuration parameters, we can make use of `|@|`.
     *
     *  Example:
     *
     *  {{{
     *   final case class Config(userName: String, port: Int)
     *
     *   object Config {
     *      val dbConfig: ConfigDescriptor[Config] =
     *         (string("USERNAME") |@| int("PORT"))(Config.apply, Config.unapply)
     *   }
     *
     *  }}}
     *
     *  Details:
     *
     *  {{{
     *     (string("USERNAME") |@| int("PORT"))(Config.apply, Config.unapply)
     *  }}}
     *
     *  is equal to
     *
     *  {{{
     *    (string("USERNAME") |@| int("PORT")).apply((a, b) => Config.apply(a, b), Config.unapply)
     *  }}}
     *
     */
    final def |@|[B](
      that: => ConfigDescriptor[B]
    ): ProductBuilder[LazyConfigDescriptor, ConfigDescriptor, A, B] =
      new ProductBuilder[LazyConfigDescriptor, ConfigDescriptor, A, B] {

        override def zip[X, Y]: (LazyConfigDescriptor[X], LazyConfigDescriptor[Y]) => LazyConfigDescriptor[(X, Y)] =
          (a, b) => thunk(a.value.zip(b.value))

        override def xmapEither[X, Y]
          : (LazyConfigDescriptor[X], X => Either[String, Y], Y => Either[String, X]) => LazyConfigDescriptor[Y] =
          (a, b, c) => thunk(a.value.xmapEither(b, c))

        override def pack[X](
          x: => ConfigDescriptor[X]
        ): LazyConfigDescriptor[X] = thunk(x)
        override def unpack[X](
          x: LazyConfigDescriptor[X]
        ): ConfigDescriptor[X] = x.value

        override val a: LazyConfigDescriptor[A] = thunk(self)
        override val b: LazyConfigDescriptor[B] = thunk(that)
      }

    /**
     * `<>` is an alias to function `orElse`.
     * This is used to represent fall-back logic when we describe config retrievals.
     *
     * Example:
     *
     *   {{{
     *     val config: ConfigDescriptor[String] = string("token") <> string("password")
     *   }}}
     *
     * This is a description that represents the following:
     * Try to retrieve the value of a parameter called "token", or else try to retrieve the value of parameter called "password"
     *
     * We know `ConfigDescriptor` is a program that describes the retrieval of a set of configuration parameters.
     * In the below example, we can either depend on a configuration called `password` or a `token` both being of the same type, in this case, a String.
     *
     *   Example:
     *
     * {{{
     *
     *   final case class Config(tokenOrPassword: String, port: Int)
     *
     *   object Config {
     *     val databaseConfig: ConfigDescriptor[Config] =
     *       (string("token") <> string("password")  |@| int("PORT"))(Config.apply, Config.unapply)
     *   }
     *
     * }}}
     */
    final def <>(that: => ConfigDescriptor[A]): ConfigDescriptor[A] =
      self orElse that

    final def <*>[B](that: => ConfigDescriptor[B]): ConfigDescriptor[(A, B)] =
      self zip that

    final def <+>[B](
      that: => ConfigDescriptor[B]
    ): ConfigDescriptor[Either[A, B]] =
      self orElseEither that

    final def default(value: A): ConfigDescriptor[A] =
      Default(thunk(self), value) ?? s"default value: $value"

    /**
     * `describe` function allows us to inject additional documentation to the configuration parameters.
     *
     * Example:
     *
     *  {{{ val port = int("PORT") ?? "database port" }}}
     *
     * A more detailed example:
     *
     * Here is a program that describes (or a ConfigDescriptor that represents) reading a `USERNAME` which is a String and `PORT` which is an Int,
     * and load it to a case class `Config`
     *
     *  {{{
     *     final case class Config(userName: String, port: Int)
     *
     *     object Config {
     *        val dbConfig: ConfigDescriptor[Config] =
     *           (string("USERNAME") |@| int("PORT"))(Config.apply, Config.unapply)
     *     }
     *  }}}
     *
     *   Later on you decided to annotate each one of them with extra documentation, which is later seen in error messages if config retrieval
     *   is a failure, and it's also used while documenting your configuration using `ConfigDocsModule`
     *
     *  {{{
     *    val dbConfigWithDoc: ConfigDescriptor[Config] =
     *       (string("USERNAME") ?? "db username" |@| int("PORT") ?? "db port")(Config.apply, Config.unapply)
     *  }}}
     *
     *  If you try and read this config from an empty source, it emits an error message with the details you provided.
     *
     *   {{{
     *     import zio.config._, ConfigDescriptor._
     *
     *     println(
     *        read(Config.databaseConfig from ConfigSource.fromMap(Map.empty))
     *      )
     *   }}}
     *
     *  returns:
     *
     *  {{{
     *   ╥
     *   ╠══╦══╗
     *   ║  ║  ║
     *   ║  ║  ╠─MissingValue
     *   ║  ║  ║ path: PORT
     *   ║  ║  ║ Details: db port, value of type int
     *   ║  ║  ▼
     *   ║  ║
     *   ║  ╠─MissingValue
     *   ║  ║ path: USERNAME
     *   ║  ║ Details: db username, value of type string
     *   ║  ▼
     *   ▼
     *
     *  }}}
     *
     *  Or, you can also use a common documentation for an entire set of config parameters.
     *
     *  {{{
     *    val detailedConfigDescriptor: ConfigDescriptor[Config] =
     *      configDescriptor ?? "Configuration related to database"
     *  }}}
     *
     */
    final def describe(description: String): ConfigDescriptor[A] =
      Describe(thunk(self), description)

    /**
     * Attach a source to the `ConfigDescriptor`.
     *
     * Example: {{{ string("PORT") from ConfigSource.fromMap(Map.empty) }}}
     *
     * Details:
     */
    final def from(that: ConfigSource): ConfigDescriptor[A] =
      self.updateSource(_.orElse(that))

    def mapKey(f: K => K): ConfigDescriptor[A] = {
      def loop[B](config: ConfigDescriptor[B]): ConfigDescriptor[B] =
        config match {
          case Source(source, propertyType) => Source(source, propertyType)
          case DynamicMap(source, conf)     => DynamicMap(source, conf.map(loop))
          case Nested(source, path, conf) =>
            Nested(source, f(path), conf.map(loop))
          case Optional(conf)           => Optional(conf.map(loop))
          case Sequence(source, conf)   => Sequence(source, conf.map(loop))
          case Describe(conf, message)  => Describe(conf.map(loop), message)
          case Default(value, value2)   => Default(value.map(loop), value2)
          case XmapEither(config, f, g) => XmapEither(config.map(loop), f, g)
          case Zip(conf1, conf2)        => Zip(conf1.map(loop), conf2.map(loop))
          case OrElseEither(value1, value2) =>
            OrElseEither(value1.map(loop), value2.map(loop))
          case OrElse(value1, value2) =>
            OrElse(value1.map(loop), value2.map(loop))
        }

      loop(self)
    }

    /**
     * `optional` function allows us to tag a configuration parameter as optional. It implies, even if it's missing configuration will be a success.
     *
     * Example:
     *
     *  {{{ val port: ConfigDescriptor[Option[Int]] = int("PORT").optional }}}
     *
     * A more detailed example:
     *
     * Here is a program that describes (or a ConfigDescriptor that represents) reading a `USERNAME` which is a String and `PORT` which is an Int,
     * and load it to a case class `Config`
     *
     *  {{{
     *     final case class Config(userName: String, port: Option[Int])
     *
     *     object Config {
     *        val dbConfig: ConfigDescriptor[Config] =
     *           (string("USERNAME") |@| int("PORT").optional)(Config.apply, Config.unapply)
     *     }
     *  }}}
     *
     *
     *   The fact that it is an optional in error messages if config retrieval
     *   is a failure, and it's also used while documenting your configuration using `ConfigDocsModule`
     *
     *  {{{
     *    val dbConfigWithDoc: ConfigDescriptor[Config] =
     *       (string("USERNAME") ?? "db username" |@| int("PORT") ?? "db port")(Config.apply, Config.unapply)
     *  }}}
     *
     *  If you try and read this config from an empty source, it emits an error message with the details you provided.
     *
     *   {{{
     *     import zio.config._, ConfigDescriptor._
     *
     *     val source = ConfigSource.fromMap(Map("USERNAME" -> "af"))
     *
     *     println(
     *        read(Config.databaseConfig from source)
     *      )
     *   }}}
     *
     *  returns:
     *
     *  {{{
     *     Config("af", None)
     *  }}}
     *
     *  Similarly,
     *
     *  {{{
     *    val source = ConfigSource.fromMap(Map("USERNAME" -> "af", "PORT" -> "8888"))
     *
     *    println(
     *      read(Config.databaseConfig from source)
     *    )
     *
     *  }}}
     *
     *  returns:
     *
     *  {{{
     *    Config("af", Some(8888))
     *  }}}
     *
     *  However, if you have given `PORT`, but it's not an integer, then it fails giving you the error details. It will also
     *  specify the fact that the parameter is an optional parameter, giving you an indication that you can either fix the parameter,
     *  or you can completely skip this parameter.
     *
     *  Example:
     *
     *   {{{
     *     import zio.config._, ConfigDescriptor._
     *
     *     val source = ConfigSource.fromMap(Map("USERNAME" -> "af", "PORT" -> "abc"))
     *
     *     println(
     *        read(Config.databaseConfig from source)
     *      )
     *   }}}
     *
     *   returns:
     *
     *  {{{
     *
     *   ╥
     *   ╠══╗
     *   ║  ║
     *   ║  ╠─FormatError
     *   ║  ║ cause: Provided value is abc, expecting the type int
     *   ║  ║ path: PORT
     *   ║  ▼
     *   ▼
     *
     *  }}}
     *
     *  Another interesting behaviour, but we often forget about optional parameters is when there is
     *  a presence of a part of the set of the config parameters
     *  representing a product, where the product itself is optional.
     *
     *  Example:
     *
     *  {{{
     *    final case class DbConfig(port: Int, host: String)
     *
     *    object DbConfig {
     *      val dbConfig: ConfigDescriptor[Option[DbConfig]] =
     *        (int("PORT") |@| string("HOST"))(DbConfig.apply, DbConfig.unapply).optional
     *    }
     *
     *  }}}
     *
     *  In this case if "PORT" is present in the source, but "HOST" is absent, then config retrieval will be a failure and not `None`.
     *  Similarly, if "HOST" is present but "PORT" is absent, the config retrieval will be a failure and not `None`.
     *
     *  If both of the parameters are absent in the source, then the config retrieval will be a success and the output will be
     *  `None`. If both of them is present, then output will be `Some(DbConfig(..))`
     */
    final def optional: ConfigDescriptor[Option[A]] =
      Optional(thunk(self)) ?? "optional value"

    /**
     * `orElse` is used to represent fall-back logic when we describe config retrievals.
     *
     * Example:
     *
     *   {{{
     *     val config: ConfigDescriptor[String] = string("token") <> string("password")
     *   }}}
     *
     * This is a description that represents the following:
     * Try to retrieve the value of a parameter called "token", or else try to retrieve the value of parameter called "password"
     *
     * We know `ConfigDescriptor` is a program that describes the retrieval of a set of configuration parameters.
     * In the below example, we can either depend on a configuration called `password` or a `token` both being of the same type, in this case, a String.
     *
     *   Example:
     *
     * {{{
     *
     *   final case class Config(tokenOrPassword: String, port: Int)
     *
     *   object Config {
     *     val databaseConfig: ConfigDescriptor[Config] =
     *       (string("token") <> string("password")  |@| int("PORT"))(Config.apply, Config.unapply)
     *   }
     *
     * }}}
     *
     * Note: `orElse` is different from `orElseEither`.
     *
     * While `orElse` fall back to parameter which is of the same type of the original config parameter,
     * `orElseEither` can fall back to a different type giving us `Either[A, B]`.
     */
    final def orElse(that: => ConfigDescriptor[A]): ConfigDescriptor[A] =
      OrElse(thunk(self), thunk(that))

    final def orElseEither[B](
      that: => ConfigDescriptor[B]
    ): ConfigDescriptor[Either[A, B]] =
      OrElseEither(thunk(self), thunk(that))

    final def unsourced: ConfigDescriptor[A] =
      self.updateSource(_ => ConfigSourceFunctions.empty)

    final def updateSource(
      f: ConfigSource => ConfigSource
    ): ConfigDescriptor[A] = {
      def loop[B](config: ConfigDescriptor[B]): ConfigDescriptor[B] =
        config match {
          case Source(source, propertyType) => Source(f(source), propertyType)
          case DynamicMap(source, conf)     => DynamicMap(f(source), conf.map(loop))
          case Nested(source, path, conf) =>
            Nested(f(source), path, conf.map(loop))
          case Optional(conf)          => Optional(conf.map(loop))
          case Sequence(source, conf)  => Sequence(f(source), conf.map(loop))
          case Describe(conf, message) => Describe(conf.map(loop), message)
          case Default(value, value2)  => Default(value.map(loop), value2)
          case XmapEither(conf, f, g)  => XmapEither(conf.map(loop), f, g)
          case Zip(conf1, conf2)       => Zip(conf1.map(loop), conf2.map(loop))
          case OrElseEither(value1, value2) =>
            OrElseEither(value1.map(loop), value2.map(loop))
          case OrElse(value1, value2) =>
            OrElse(value1.map(loop), value2.map(loop))
        }

      loop(self)
    }

    final def transform[B](to: A => B, from: B => A): ConfigDescriptor[B] =
      self.xmapEither(a => Right(to(a)), b => Right(from(b)))

    final def transformEither[B](
      f: A => Either[String, B],
      g: B => Either[String, A]
    ): ConfigDescriptor[B] =
      self.xmapEither(f, g)

    final def transformEitherLeft[B](f: A => Either[String, B], g: B => A): ConfigDescriptor[B] =
      self.transformEitherLeft(f)(g)(identity)

    final def transformEitherLeft[E, B](
      f: A => Either[E, B]
    )(g: B => A)(h: E => String): ConfigDescriptor[B] =
      self.xmapEither[E, B](f)(b => Right(g(b)))(h)

    final def transformEitherRight[B](
      f: A => B,
      g: B => Either[String, A]
    ): ConfigDescriptor[B] =
      self.transformEitherRight(f)(g)(identity)

    final def transformEitherRight[E, B](
      f: A => B
    )(g: B => Either[E, A])(h: E => String): ConfigDescriptor[B] =
      self.xmapEither[E, B](t => Right(f(t)))(g)(h)

    final def xmap[B](to: A => B, from: B => A): ConfigDescriptor[B] =
      self.xmapEither(a => Right(to(a)), b => Right(from(b)))

    final def xmapEither[B](f: A => Either[String, B], g: B => Either[String, A]): ConfigDescriptor[B] =
      XmapEither(thunk(self), f, g)

    final def xmapEither[E, B](
      f: A => Either[E, B]
    )(g: B => Either[E, A])(h: E => String): ConfigDescriptor[B] =
      self.xmapEither[B](
        (a: A) => ((f(a): Either[E, B]).swap: Either[B, E]).map(h).swap,
        (b: B) => ((g(b): Either[E, A]).swap: Either[A, E]).map(h).swap
      )

    final def xmapEither2[B, C](that: => ConfigDescriptor[B])(
      f: (A, B) => Either[String, C],
      g: C => Either[String, (A, B)]
    ): ConfigDescriptor[C] =
      (self |@| that)
        .apply[(A, B)](Tuple2.apply, Tuple2.unapply)
        .xmapEither({ case (a, b) => f(a, b) }, g)

    final def zip[B](that: => ConfigDescriptor[B]): ConfigDescriptor[(A, B)] =
      Zip(thunk(self), thunk(that))
  }

  trait ConfigDescriptorFunctions {

    def collectAll[A](head: ConfigDescriptor[A], tail: ConfigDescriptor[A]*): ConfigDescriptor[List[A]] =
      sequence(head, tail: _*)(
        { case (a, t) => a :: t },
        l => l.headOption.map(h => (h, l.tail))
      )

    def lazyCollectAll[A](
      head: LazyConfigDescriptor[A],
      tail: LazyConfigDescriptor[A]*
    ): ConfigDescriptor[List[A]] =
      lazySequence(head, tail: _*)(
        { case (a, t) => a :: t },
        l => l.headOption.map(h => (h, l.tail))
      )

    def head[A](desc: ConfigDescriptor[A]): ConfigDescriptor[A] =
      desc.orElse(
        list(desc)
          .xmapEither[A](
            _.headOption
              .fold[Either[String, A]](Left("Element is missing"))(Right(_)),
            v => Right(v :: Nil)
          )
      )

    def head[A](path: K)(desc: => ConfigDescriptor[A]): ConfigDescriptor[A] =
      nested(path)(head(desc))

    def list[K, V, A](desc: => ConfigDescriptor[A]): ConfigDescriptor[List[A]] =
      Sequence(ConfigSourceFunctions.empty, thunk(desc))

    def list[A](
      path: K
    )(desc: => ConfigDescriptor[A]): ConfigDescriptor[List[A]] =
      nested(path)(list(desc))

    def listOrSingleton[A](
      path: K
    )(desc: => ConfigDescriptor[A]): ConfigDescriptor[List[A]] =
      list(path)(desc) orElse (
        nested(path)(desc)
          .xmapEither[List[A]](
            value => Right(List(value)),
            _.headOption match {
              case Some(value) => Right(value)
              case None        => Left("Cannot write an empty list back")
            }
          )
        )

    def map[A](desc: => ConfigDescriptor[A]): ConfigDescriptor[Map[K, A]] =
      DynamicMap(ConfigSourceFunctions.empty, thunk(desc))

    def map[A](
      path: K
    )(desc: => ConfigDescriptor[A]): ConfigDescriptor[Map[K, A]] =
      nested(path)(map(desc))

    def nested[A](path: K)(desc: => ConfigDescriptor[A]): ConfigDescriptor[A] =
      Nested(ConfigSourceFunctions.empty, path, thunk(desc))

    def sequence[A](
      head: => ConfigDescriptor[A],
      tail: ConfigDescriptor[A]*
    ): ConfigDescriptor[(A, List[A])] =
      lazySequence(thunk(head), tail.map(a => thunk(a)): _*)

    def lazySequence[A](
      head: LazyConfigDescriptor[A],
      tail: LazyConfigDescriptor[A]*
    ): ConfigDescriptor[(A, List[A])] =
      tail.reverse.foldLeft[ConfigDescriptor[(A, List[A])]](
        head.value.xmap((a: A) => (a, Nil), (b: (A, List[A])) => b._1)
      )(
        (b: ConfigDescriptor[(A, List[A])], a: LazyConfigDescriptor[A]) =>
          b.xmapEither2(a.value)({
            case ((first, tail), a) => Right((first, a :: tail))
          }, {
            case (_, Nil)              => Left("Invalid list length")
            case (first, head :: tail) => Right(((first, tail), head))
          })
      )

    def set[K, V, A](desc: => ConfigDescriptor[A]): ConfigDescriptor[Set[A]] =
      list(desc).xmapEither(distinctListToSet, s => Right(s.toList))

    def set[A](
      path: K
    )(desc: => ConfigDescriptor[A]): ConfigDescriptor[Set[A]] =
      nested(path)(set(desc))

    private def distinctListToSet[A](list: List[A]): Either[String, Set[A]] =
      if (list.size == list.distinct.size) Right(list.toSet)
      else Left("Duplicated values found")

  }

  case class LazyConfigDescriptor[A](
    private val get: () => ConfigDescriptor[A]
  ) {
    def value: ConfigDescriptor[A] = get()
    def map[B](
      f: ConfigDescriptor[A] => ConfigDescriptor[B]
    ): LazyConfigDescriptor[B] =
      LazyConfigDescriptor(() => f(get()))
  }

  final def thunk[A](config: => ConfigDescriptor[A]): LazyConfigDescriptor[A] =
    LazyConfigDescriptor(() => config)

  def dump[A](config: ConfigDescriptor[A]): String = {
    val builder = new StringBuilder

    def go[A](config: ConfigDescriptor[A], prefix: String): Unit =
      config match {
        case Default(config, default) =>
          builder.append(s"${prefix}Default($default}\n")
          go(config.value, prefix + "  ")
        case Describe(config, message) =>
          builder.append(s"${prefix}Describe($message}\n")
          go(config.value, prefix + "  ")
        case DynamicMap(_, config) =>
          builder.append(s"${prefix}DynamicMap\n")
          go(config.value, prefix + "  ")
        case Nested(_, path, config) =>
          builder.append(s"${prefix}Nested($path)\n")
          go(config.value, prefix + "  ")
        case Optional(config) =>
          builder.append(s"${prefix}Optional\n")
          go(config.value, prefix + "  ")
        case OrElse(left, right) =>
          builder.append(s"${prefix}OrElse\n")
          go(left.value, prefix + "  ")
          go(right.value, prefix + "  ")
        case OrElseEither(left, right) =>
          builder.append(s"${prefix}OrElseEither\n")
          go(left.value, prefix + "  ")
          go(right.value, prefix + "  ")
        case Sequence(_, config) =>
          builder.append(s"${prefix}Sequence\n")
          go(config.value, prefix + "  ")
        case Source(_, propertyType) =>
          builder.append(s"${prefix}Source($propertyType)\n")
          ()
        case Zip(left, right) =>
          builder.append(s"${prefix}Zip\n")
          go(left.value, prefix + "  ")
          go(right.value, prefix + "  ")
        case XmapEither(config, _, _) =>
          go(config.value, prefix)
      }
    go(config, "")
    builder.toString()
  }

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
