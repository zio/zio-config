package zio.config

import VersionSpecificSupport._

trait ConfigDescriptorModule extends ConfigSourceModule { module =>
  import ConfigDescriptorAdt._

  sealed trait ConfigDescriptor[A] { self =>

    /**
     * Given `A` and `B` the below `apply` function is used to
     * convert a `ConfigDescriptor[A]` to `ConfigDescriptor[B]`.
     *
     * While we can retrieve B directly from A,
     * there is no guarantee that reverse relationship exists. i.e, B => A.
     *
     * Instead the reverse relationship is `B => Option[A]`.
     * This is why the arguments of this function are `app: A => B`
     * and `unapp: B => Option[A]`
     *
     * Let's define a simple `ConfigDescriptor`,
     * that talks about retrieving a `String` configuration (example: PORT).
     *
     *  {{{
     *    val port: ConfigDescriptor[Int] = int("PORT")
     *     // NOTE: We are not attaching a real source here,
     *     // because a config descriptor can exist without a source
     *  }}}
     *
     * Later you decided to convert the type of `Port` from `Int` to `String`
     * for a variety of reasons in your application.
     *
     * In this case. In this case, `A => B` is `Int => String` which will always work.
     *
     * However, the reverse relationship (which is used to retrieve
     * the original type from its transformed representation) is `String => Int` is
     * not a total function, unless it was `String => Either[error, Int]`.
     *
     * That is, Not all elements of set `String` can be converted to `Int`.
     * Example: A string "abc" cannot be converted to `Int`.
     *
     * Hence we can do `s => Try(s.toInt).toOption` to mark the possibility of errors.
     * This is a function of the type: `B => Option[A]`.
     *
     * Also note that, you can make use of `transformEither`
     * instead of `apply` which is a much more powerful method to
     * convert from one type to the other keeping the error information
     * without falling back to `Option`.
     *
     *  {{{
     *
     *    val portString: ConfigDescriptor[Int] =
     *       port.apply[String](_.toString, (s: String) => Try(s.toInt).toOption)
     *
     *  }}}
     *
     *   With the above information you can read a port from a source and convert to a string,
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
     *  Now given a stringified port "8888", you can also write it
     *  back to the source safely (i.e, guaranteed that the read will work) because
     *  the it runs through `(s: String) => Try(s.toInt).toOption`
     *  and verify it is a real port that can be converted to `Int`.
     *
     *  {{{
     *
     *     import zio.config.typesafe._
     *      // as toJson is available only through zio-config-typesafe module
     *
     *     val writtenBack: Either[String, PropertyTree[String, String]] =
     *       write(portString, "8888")
     *
     *     val jsonRepr: Either[String, String] =
     *       writtenBack.map(_.toJson) // { "port" : "8888" }
     *
     *     val mapRepr: Either[String, Map[String, String]] =
     *       writtenBack.map(_.flattenString()) // Map("port" -> "8888")
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
     * `??` is an alias to `describe` which allows us to inject additional
     * documentation to the configuration parameters.
     *
     * Example:
     *
     *  {{{ val port = int("PORT") ?? "database port" }}}
     *
     * A more detailed example:
     *
     * Here is a program that describes (or a ConfigDescriptor that represents)
     * reading a `USERNAME` which is a String and `PORT` which is an Int,
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
     *   Later on you decided to annotate each one of them with extra documentation,
     *   which is later seen in error messages if config retrieval
     *   is a failure, and it's also used while documenting your configuration
     *   using `ConfigDocsModule`
     *
     *  {{{
     *    val dbConfigWithDoc: ConfigDescriptor[Config] =
     *       (string("USERNAME") ?? "db username" |@|
     *          int("PORT") ?? "db port"
     *         )(Config.apply, Config.unapply)
     *  }}}
     *
     *  If you try and read this config from an empty source,
     *  it emits an error message with the details you provided.
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
     * |@| is a ConfigDescriptor builder. We know `ConfigDescriptor`
     * is a program that describes the retrieval of a set of configuration parameters.
     *
     *  Below given is a `ConfigDescriptor` that describes the retrieval of a single config.
     *
     *  {{{
     *    val port: ConfigDescriptor[String] = string("PORT")
     *  }}}
     *
     *  However, in order to retrieve multiple configuration parameters,
     *  we can make use of `|@|`.
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
     * Try to retrieve the value of a parameter called "token",
     * or else try to retrieve the value of parameter called "password"
     *
     * We know `ConfigDescriptor` is a program that describes the retrieval
     * of a set of configuration parameters.
     *
     * In the below example, we can either depend on a configuration called
     * `password` or a `token` both being of the same type, in this case, a String.
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

    /**
     * `<*>` is an alias to function `zip`.
     * This is used to represent retrieving the config as a tuple.
     *
     * Example:
     *
     *  {{{
     *
     *    val config: ConfigDescriptor[(String, Int)] = string("URL") <*> int("PORT")
     *
     *  }}}
     *
     * This is a description that represents the following:
     * Retrieve values of URL and PORT which are String and Int respectively, and return a tuple.
     *
     * The above description is equivalent to
     *
     *  {{{
     *
     *    val config2: ConfigDescriptor[(String, Int)] = (string("URL") |@| int("PORT")).tupled
     *
     *  }}}
     *
     *  Using `|@|` over `<>` avoids nested tuples.
     *
     */
    final def <*>[B](that: => ConfigDescriptor[B]): ConfigDescriptor[(A, B)] =
      self zip that

    /**
     * `<+>` is an alias to function `orElseEither`.
     * This is used to represent fall-back logic when we describe config retrievals. Unlike `orElse`, the
     * the fall-back config parameter can have a different type in `orElseEither`.
     *
     * Example:
     *
     *   {{{
     *
     *     val config: ConfigDescriptor[Either[Int, String]] = int("MONTH") <+> string("MONTH")
     *
     *   }}}
     *
     * This is a description that represents the following:
     * Try to retrieve the value of a MONTH as an `Int`, and if there is a format error, try and retrieve it as a `String`.
     *
     * Detail:
     *
     * We know `ConfigDescriptor` is a program that describes the retrieval of a set of configuration parameters.
     * In the below example, we can either depend on a configuration called `password`
     * or a `token` both being of the same type, in this case, a String.
     *
     *   Example:
     *
     *   Given:
     *
     *  {{{
     *
     *    final case class BasicAuth(username: String, password: String)
     *    final case class OAuth(clientId: String, secret: String)
     *
     *    val basicAuth: ConfigDescriptor[BasicAuth] =
     *      (string("USERNAME") |@| string("PASSWORD"))(BasicAuth.apply, BasicAuth.unapply)
     *
     *    val oAuth: ConfigDescriptor[OAuth] =
     *      (string("CLIENT_ID") |@| string("SECRET"))(OAuth.apply, OAuth.unapply)
     *
     *    val myConfig: ConfigDescriptor[Either[BasicAuth, OAuth]] =
     *      basicAuth <+> oAuth
     *
     *  }}}
     *
     *  then,
     *
     *  {{{
     *
     *     val source = ConfigSource.fromMap(Map("USERNAME" -> "abc", "PASSWORD" -> "cde")
     *
     *     read(myConfig from source)
     *
     *  }}}
     *
     *  returns:
     *
     *  {{{
     *
     *    Left(BasicAuth("abc", "def")
     *
     *  }}}
     *
     *  Similarly,
     *
     *  {{{
     *
     *     val source = ConfigSource.fromMap(Map("CLIENT_ID" -> "xyz", "SECRET" -> "afg==")
     *
     *     read(myConfig from source)
     *
     *  }}}
     *
     *  returns:
     *
     *  {{{
     *
     *    Right(OAuth("xyz", "afg==")
     *
     *  }}}
     *
     */
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
     * `optional` function allows us to tag a configuration parameter as optional.
     * It implies, even if it's missing configuration will be a success.
     *
     * Example:
     *
     *  {{{ val port: ConfigDescriptor[Option[Int]] = int("PORT").optional }}}
     *
     * A more detailed example:
     *
     * Here is a program that describes (or a ConfigDescriptor that represents) reading a `USERNAME`
     * which is a String and `PORT` which is an Int,
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
     *  However, if you have given `PORT`, but it's not an integer,
     *  then it fails giving you the error details.
     *
     *  Within the error message, it will also specify the fact
     *  that the parameter is an optional parameter,
     *  giving you an indication that you can either fix the parameter,
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
     *  In this case if "PORT" is present in the source, but "HOST" is absent,
     *  then config retrieval will be a failure and not `None`.
     *  Similarly, if "HOST" is present but "PORT" is absent,
     *  the config retrieval will be a failure and not `None`.
     *
     *  If both of the parameters are absent in the source, then the
     *  config retrieval will be a success and the output will be
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
     *
     * `orElse` will be useful in retrieving configuration that are represented as coproducted (sealed trait). However,
     * it may become fairly verbose, such that usage `zio-config-magnolia` to derive the config automatically, will become a reasonable
     * alternative.
     */
    final def orElse(that: => ConfigDescriptor[A]): ConfigDescriptor[A] =
      OrElse(thunk(self), thunk(that))

    /**
     * `orElseEither` is used to represent fall-back logic when we describe config retrievals. Unlike `orElse`,
     * the fall-back config parameter can have a different type in `orElseEither`.
     *
     * Example:
     *
     *   {{{
     *
     *     val config: ConfigDescriptor[Either[Int, String]] = int("MONTH") <+> string("MONTH")
     *
     *   }}}
     *
     * This is a description that represents the following:
     * Try to retrieve the value of a MONTH as an `Int`, and if there is a format error, try and retrieve it as a `String`.
     *
     * Detail:
     *
     * We know `ConfigDescriptor` is a program that describes the retrieval of a set of configuration parameters.
     * In the below example, we can either depend on a configuration called `password`
     * or a `token` both being of the same type, in this case, a String.
     *
     *   Example:
     *
     *   Given:
     *
     *  {{{
     *
     *    final case class BasicAuth(username: String, password: String)
     *    final case class OAuth(clientId: String, secret: String)
     *
     *    val basicAuth: ConfigDescriptor[BasicAuth] =
     *      (string("USERNAME") |@| string("PASSWORD"))(BasicAuth.apply, BasicAuth.unapply)
     *
     *    val oAuth: ConfigDescriptor[OAuth] =
     *      (string("CLIENT_ID") |@| string("SECRET"))(OAuth.apply, OAuth.unapply)
     *
     *    val myConfig: ConfigDescriptor[Either[BasicAuth, OAuth]] =
     *      basicAuth <+> oAuth
     *
     *  }}}
     *
     *  then,
     *
     *  {{{
     *
     *     val source = ConfigSource.fromMap(Map("USERNAME" -> "abc", "PASSWORD" -> "cde")
     *
     *     read(myConfig from source)
     *
     *  }}}
     *
     *  returns:
     *
     *  {{{
     *
     *    Left(BasicAuth("abc", "def")
     *
     *  }}}
     *
     *  Similarly,
     *
     *  {{{
     *
     *     val source = ConfigSource.fromMap(Map("CLIENT_ID" -> "xyz", "SECRET" -> "afg==")
     *
     *     read(myConfig from source)
     *
     *  }}}
     *
     *  returns:
     *
     *  {{{
     *
     *    Right(OAuth("xyz", "afg==")
     *
     *  }}}
     *
     */
    final def orElseEither[B](
      that: => ConfigDescriptor[B]
    ): ConfigDescriptor[Either[A, B]] =
      OrElseEither(thunk(self), thunk(that))

    /**
     * Untag all sources associated with a `ConfigDescriptor`.
     *
     *  As we know `ConfigDescriptor` represents a program that describes the retrieval of config parameters.
     *  In fact, the same program can be used to write back the config in various shapes.
     *
     *  Either case, a `ConfigDescriptor` can exist without a `Source` attached.
     *
     *  Example:
     *
     *  {{{
     *    val stringConfig: ConfigDescriptor[String] = string("USERNAME")
     *  }}}
     *
     *  Later on we can read the config by attaching a source.
     *
     *  {{{
     *
     *    val result = read(stringConfig from ConfigSource.fromMap(Map.empty))
     *
     *  }}}
     *
     *  However, you can attach a source to the configDescriptor at an earlier stage.
     *
     *  For example:
     *
     *  {{{
     *
     *    val stringConfig: ConfigDescriptor[String] =
     *      string("USERNAME") from ConfigSource.fromMap(Map.empty)
     *
     *  }}}
     *
     *  Later on, you can simply read it using:
     *
     *  {{{
     *     val result = read(stringConfig)
     *  }}}
     *
     *  Using `unsourced`, you can now untag the source from `stringConfig`.
     *
     *  {{{
     *
     *    val stringConfigNoSource: ConfigDescriptor[String] =
     *      stringConfig.unsourced
     *  }}}
     *
     *  This can be useful in test cases where you want to remove a source and attach a different source.
     *
     *  Example:
     *
     *  {{{
     *
     *     val testSource: ConfigSource = ConfigSource.fromMap(Map(..))
     *
     *     val result = stringConfig.unsourced from testSource
     *
     *  }}}
     */
    final def unsourced: ConfigDescriptor[A] =
      self.updateSource(_ => ConfigSourceFunctions.empty)

    /**
     * `updateSource` can update the source of an existing `ConfigDescriptor`
     *
     * Example:
     *
     *   {{{
     *
     *      val configSource1 = ConfigSource.fromMap(Map.empty)
     *      val configSource2 = ConfigSource.fromMap(Map("USERNAME" -> "abc"))
     *
     *      val config = string("USERNAME") from configSource1
     *
     *      val updatedConfig = config updateSource (_ orElse configSource2)
     *
     *   }}}
     *
     * In the above example, we update the existing ConfigDescriptor to try another ConfigSource called configSource2,
     * if it fails to retrieve the value of USERNAME from configSource1.
     */
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

    /**
     * Given `A` and `B`, `f: A => B`, and `g: B => A`, then
     * `transform` allows us to transform a `ConfigDescriptor[A]` to `ConfigDescriptor[B]`.
     *
     *
     * Example :
     *  `transform` is useful especially when you define newtypes.
     *
     *  {{{
     *    final case class Port(port: Int) extends AnyVal
     *
     *    val config: ConfigDescriptor[Port] =
     *      int("PORT").transform[Port](Port.apply, _.int)
     *  }}}
     *
     *  While `to: A => B` (in this case, `Int => Port`) is used to read to a `Port` case class,
     *  `from: B => A` (which is, `Port => Int`) is used when we want to write `Port` directly to a source representation.
     *
     *  Example:
     *
     *  {{{
     *
     *     import zio.config.typesafe._ // as toJson is available only through zio-config-typesafe module
     *
     *     val writtenBack: Either[String, PropertyTree[String, String]] = write(config, Port(8888))
     *
     *     val jsonRepr: Either[String, String] = writtenBack.map(_.toJson) // { "port" : "8888" }
     *     val mapRepr: Either[String, Map[String, String]] = writtenBack.map(_.flattenString()) // Map("port" -> "8888")
     *  }}}
     */
    final def transform[B](to: A => B, from: B => A): ConfigDescriptor[B] =
      self.xmapEither(a => Right(to(a)), b => Right(from(b)))

    /**
     * `transformEither` is an alias to `xmapEither`.
     * Given `A` and `B`, `transformEither` function is used to convert a `ConfigDescriptor[A]` to `ConfigDescriptor[B]`.
     *
     * It is important to note that both `to` and `fro` is fallible, allowing us to represent
     * almost all possible relationships.
     *
     * Example:
     *
     * Let's define a simple `ConfigDescriptor`, that talks about retrieving a `S3Path` ( a bucket and prefix in AWS s3).
     * Given you want to retrieve an S3Path from ConfigSource. Given a string, converting it to S3Path can fail, and even converting
     * S3Path to a String can fail as well.
     *
     *  {{{
     *    import java.time.DateTimeFormatter
     *    import java.time.LocalDate
     *
     *    final case class S3Path(bucket: String , prefix: String, partition: LocalDate) {
     *      def convertToString(partitionPattern: String): Either[String, String] =
     *        Try { DateTimeFormatter.ofPattern(partitionPattern).format(partition) }.toEither
     *          .map(dateStr => s"\${bucket}/\${prefix}/\${dateStr}").swap.map(_.getMessage).swap
     *    }
     *
     *    object S3Path {
     *      def fromStr(s3Path: String): Either[String, S3Path] = {
     *        val splitted = s3Path.split("/").toList
     *
     *        if (splitted.size > 3)
     *          Left("Invalid s3 path")
     *        else
     *          for {
     *             bucket <- splitted.headOption.toRight("Empty s3 path")
     *             prefix <- splitted.lift(1).toRight("Invalid prefix, or empty prefix in s3 path")
     *             partition <- splitted.lift(2).toRight("Empty partition").flatMap(dateStr => LocalDate.parse(dateStr))
     *          } yield S3Path(bucket, prefix, partition)
     *      }
     *    }
     *
     *    val s3PathConfig: ConfigDescriptor[S3Path] =
     *      string("S3_PATH").transformEither[S3Path](S3Path.fromStr, _.convertToString("yyyy-MM-dd"))
     *
     *  }}}
     *
     */
    final def transformEither[B](
      to: A => Either[String, B],
      from: B => Either[String, A]
    ): ConfigDescriptor[B] =
      self.xmapEither(to, from)

    final def transformEitherLeft[B](f: A => Either[String, B], g: B => A): ConfigDescriptor[B] =
      self.transformEitherLeft(f)(g)(identity)

    final def transformEitherLeft[E, B](
      f: A => Either[E, B]
    )(g: B => A)(h: E => String): ConfigDescriptor[B] =
      self.xmapEitherE[E, B](f)(b => Right(g(b)))(h)

    final def transformEitherRight[B](
      f: A => B,
      g: B => Either[String, A]
    ): ConfigDescriptor[B] =
      self.transformEitherRightE(f)(g)(identity)

    final def transformEitherRightE[E, B](
      f: A => B
    )(g: B => Either[E, A])(h: E => String): ConfigDescriptor[B] =
      self.xmapEitherE[E, B](t => Right(f(t)))(g)(h)

    /**
     * `xmap` is an alias to `transform`.
     *
     * Given `A` and `B`, `f: A => B`, and `g: B => A`, then
     * `transform` allows us to transform a `ConfigDescriptor[A]` to `ConfigDescriptor[B]`.
     *
     *
     * Example :
     *  `transform` is useful especially when you define newtypes.
     *
     *  {{{
     *    final case class Port(port: Int) extends AnyVal
     *
     *    val config: ConfigDescriptor[Port] =
     *      int("PORT").transform[Port](Port.apply, _.int)
     *  }}}
     *
     *  While `to: A => B` (in this case, `Int => Port`) is used to read to a `Port` case class,
     *  `from: B => A` (which is, `Port => Int`) is used when we want to write `Port` directly to a source representation.
     *
     *  Example:
     *
     *  {{{
     *
     *     import zio.config.typesafe._ // as toJson is available only through zio-config-typesafe module
     *
     *     val writtenBack: Either[String, PropertyTree[String, String]] = write(config, Port(8888))
     *
     *     val jsonRepr: Either[String, String] = writtenBack.map(_.toJson) // { "port" : "8888" }
     *     val mapRepr: Either[String, Map[String, String]] = writtenBack.map(_.flattenString()) // Map("port" -> "8888")
     *  }}}
     */
    final def xmap[B](to: A => B, from: B => A): ConfigDescriptor[B] =
      self.xmapEither(a => Right(to(a)), b => Right(from(b)))

    /**
     * Given `A` and `B`, `xmapEither` function is used to convert a `ConfigDescriptor[A]` to `ConfigDescriptor[B]`.
     *
     * It is important to note that both `to` and `fro` is fallible, allowing us to represent
     * almost all possible relationships.
     *
     * Example:
     *
     * Let's define a simple `ConfigDescriptor`, that talks about retrieving a `S3Path` ( a bucket and prefix in AWS s3).
     * Given you want to retrieve an S3Path from ConfigSource. Given a string, converting it to S3Path can fail, and even converting
     * S3Path to a String can fail as well.
     *
     *  {{{
     *    import java.time.DateTimeFormatter
     *    import java.time.LocalDate
     *
     *     // We prefer important structures like S3Path in your application to be a well defined structure
     *     // Usage of abstract sealed case class is to disallow direct creation of `S3Path` from anywhere else other
     *     // than its companion object. This is not anything specific to zio-config.
     *
     *    final case class S3Path(bucket: String , prefix: String, partition: LocalDate) {
     *      def convertToString(partitionPattern: String): Either[String, String] =
     *        Try { DateTimeFormatter.ofPattern(partitionPattern).format(partition) }.toEither
     *          .map(dateStr => s"\${bucket}/\${prefix}/\${dateStr}").swap.map(_.getMessage).swap
     *    }
     *
     *    object S3Path {
     *      def fromStr(s3Path: String): Either[String, S3Path] = {
     *        val splitted = s3Path.split("/").toList
     *
     *        if (splitted.size > 3)
     *          Left("Invalid s3 path")
     *        else
     *          for {
     *             bucket <- splitted.headOption.toRight("Empty s3 path")
     *             prefix <- splitted.lift(1).toRight("Invalid prefix, or empty prefix in s3 path")
     *             date <- splitted.lift(2).toRight("Empty partition")
     *              partition <- Try(LocalDate.parse(date)).toEither
     *          } yield S3Path(bucket, prefix, partition)
     *      }
     *    }
     *
     *    val s3PathConfig: ConfigDescriptor[S3Path] =
     *      string("S3_PATH").xmapEither[S3Path](S3Path.fromStr, _.convertToString("yyyy-MM-dd"))
     *
     *  }}}
     *
     */
    final def xmapEither[B](f: A => Either[String, B], g: B => Either[String, A]): ConfigDescriptor[B] =
      XmapEither(thunk(self), f, g)

    /**
     * Given `A` and `B`, `xmapEitherE` function is used to
     * convert a `ConfigDescriptor[A]` to `ConfigDescriptor[B]`.
     *
     * It is important to note that both `to` and `fro` is fallible,
     * allowing us to represent almost all possible relationships.
     *
     * `xmapEitherE` is a more generalised form of `xmapEither`.
     *
     * Unlike `xmapEither`, the errors doesn't need to be a `String` when using
     * `xmapEitherE` as long as there is a way to convert `E` to `String` (`show`).
     *
     * The reason behind existence of `show` (a `String` representation of `E`)
     * is because of the following:
     *   All the errors need to be printed out to the user (console/log)
     *   during failed configuration retrievals, or during documentation.
     *
     * Unlike `xmapEither`, `xmapEitherE` is curried to make better use of Scala's type inference.
     *
     * Example:
     *
     * Let's define a simple `ConfigDescriptor`, that talks about retrieving
     * a `S3Path` ( a bucket and prefix followed by a partition director in AWS s3)
     * from (any) ConfigSource.
     *
     * Given a string at the source, converting it to S3Path can fail,
     * and even converting S3Path to a String can fail as well.
     *
     *  {{{
     *    import java.time.DateTimeFormatter
     *    import java.time.LocalDate
     *
     *    // We prefer important structures like S3Path in your application
     *    // to be a well defined structure
     *
     *    // Usage of `abstract sealed case class` is to disallow
     *    // direct creation of `S3Path` from anywhere else other
     *    // than its companion object. This is not anything specific to zio-config.
     *
     *    abstract sealed case class S3Path(bucket: String , prefix: String, partition: LocalDate) {
     *      def convertToString(partitionPattern: String): Either[Throwable, String] =
     *        Try { DateTimeFormatter.ofPattern(partitionPattern).format(partition) }.toEither
     *          .map(dateStr => s"\${bucket}/\${prefix}/\${dateStr}")
     *    }
     *
     *    object S3Path {
     *      def fromStr(s3Path: String): Either[Throwable, S3Path] = {
     *        val splitted = s3Path.split("/").toList
     *
     *        if (splitted.size > 3)
     *          Left(new RuntimeException("Invalid s3 path"))
     *        else
     *         for {
     *           bucket <- splitted.headOption.toRight(new RuntimeException("Empty s3 path"))
     *           prefix <- splitted.lift(1).toRight(new RuntimeException("Invalid prefix, or empty prefix in s3 path"))
     *           date <- splitted.lift(2).toRight(new RuntimeException("Empty partition"))
     *           partition <- Try(LocalDate.parse(date)).toEither
     *         } yield new S3Path(bucket, prefix, partition){}
     *      }
     *    }
     *
     *    val s3PathConfig: ConfigDescriptor[S3Path] =
     *      string("S3_PATH").xmapEitherE[Throwable S3Path](S3Path.fromStr)(_.convertToString("yyyy-MM-dd"))(_.getMessage)
     *
     *  }}}
     */
    final def xmapEitherE[E, B](
      to: A => Either[E, B]
    )(from: B => Either[E, A])(show: E => String): ConfigDescriptor[B] =
      self.xmapEither[B](
        (a: A) => ((to(a): Either[E, B]).swap: Either[B, E]).map(show).swap,
        (b: B) => ((from(b): Either[E, A]).swap: Either[A, E]).map(show).swap
      )

    /**
     * `xmapEither2` is similar to `xmapEither` but the function
     * is mostly used as an internal implementation
     * in zio-config. For the same reason, users hardly need `xmapEither2`.
     * Instead take a look at `xmapEither` (or `transformEither`).
     *
     * `xmapEither2` deals with retrieving two configurations represented
     * by `ConfigDescriptor[A]` and `ConfigDescriptor[B]`,
     * and corresponding `to` and `from` functions converting a tuple `(A, B)` to C` and it's
     * reverse direction, to finally form a `ConfigDescriptor[C]`.
     *
     * Those who are familiar with `Applicative` in Functional programming,
     * `xmapEither2` almost takes the form of `Applicative`:
     * `F[A] => F[B] => (A, B) => C => F[C]`.
     *
     * Implementation detail:
     * This is used to implement sequence` (`traverse`)
     * behaviour of `ConfigDescriptor[A]`
     */
    final def xmapEither2[B, C](that: => ConfigDescriptor[B])(
      to: (A, B) => Either[String, C],
      from: C => Either[String, (A, B)]
    ): ConfigDescriptor[C] =
      (self |@| that)
        .apply[(A, B)](Tuple2.apply, Tuple2.unapply)
        .xmapEither({ case (a, b) => to(a, b) }, from)

    /**
     * `zip` is used to represent retrieving the config as a tuple.
     *
     * Example:
     *
     *  {{{
     *
     *    val config: ConfigDescriptor[(String, Int)] = string("URL") <*> int("PORT")
     *
     *  }}}
     *
     * This is a description that represents the following:
     * Retrieve values of URL and PORT which are String and Int respectively, and return a tuple.
     *
     * The above description is equivalent to
     *
     *  {{{
     *
     *    val config2: ConfigDescriptor[(String, Int)] = (string("URL") |@| int("PORT")).tupled
     *
     *  }}}
     *
     *  Using `|@|` over `<>` avoids nested tuples.
     *
     */
    final def zip[B](that: => ConfigDescriptor[B]): ConfigDescriptor[(A, B)] =
      Zip(thunk(self), thunk(that))
  }

  trait ConfigDescriptorFunctions {

    /**
     * `collectAll` is an alias to `sequence`. In Functional Programming terms,
     * it is a Traverse implementation for ConfigDescriptor.
     * In other words, it allows us to convert a `List` of `ConfigDescriptor[A]`
     * to `ConfigDescriptor[List[A]]`.
     *
     * Example:
     *
     * {{{
     *   final case class Variables(variable1: Int, variable2: Option[Int])
     *
     *   object CollectAllExample extends App with EitherImpureOps {
     *     val listOfConfig: List[ConfigDescriptor[Variables]] =
     *       List("GROUP1", "GROUP2", "GROUP3", "GROUP4")
     *         .map(
     *           group =>
     *             (int(s"${group}_VARIABLE1") |@| int(s"${group}_VARIABLE2").optional)(Variables.apply, Variables.unapply)
     *         )
     *
     *     val configOfList: ConfigDescriptor[List[Variables]] =
     *       collectAll(listOfConfig.head, listOfConfig.tail: _*)
     *
     *     val map =
     *       Map(
     *         "GROUP1_VARIABLE1" -> "1",
     *         "GROUP1_VARIABLE2" -> "2",
     *         "GROUP2_VARIABLE1" -> "3",
     *         "GROUP2_VARIABLE2" -> "4",
     *         "GROUP3_VARIABLE1" -> "5",
     *         "GROUP3_VARIABLE2" -> "6",
     *         "GROUP4_VARIABLE1" -> "7"
     *       )
     *
     *     // loadOrThrow here is only for the purpose of example
     *     val result: List[Variables] = read(configOfList from ConfigSource.fromMap(map, "constant")).loadOrThrow
     *
     *     val written: PropertyTree[String, String] = write(configOfList, result).loadOrThrow
     *
     *     assert(
     *       result == List(Variables(1, Some(2)), Variables(3, Some(4)), Variables(5, Some(6)), Variables(7, None))
     *     )
     * }}}
     */
    def collectAll[A](head: ConfigDescriptor[A], tail: ConfigDescriptor[A]*): ConfigDescriptor[List[A]] =
      sequence(head, tail: _*)(
        { case (a, t) => a :: t },
        l => l.headOption.map(h => (h, l.tail))
      )

    /**
     * `lazyCollectAll` is a lazy version to `sequence` helping out zio-config
     * in handling recursive config structures. However, for the most import,
     * users need to interact with only sequence and collectAll. In Functional Programming terms,
     *
     * Example:
     *
     * {{{
     *   final case class Variables(variable1: Int, variable2: Option[Int])
     *
     *   object CollectAllExample extends App with EitherImpureOps {
     *     val listOfConfig: List[LazyConfigDescriptor[Variables]] =
     *       List("GROUP1", "GROUP2", "GROUP3", "GROUP4")
     *         .map(
     *           group =>
     *             thunk(int(s"${group}_VARIABLE1") |@| int(s"${group}_VARIABLE2").optional)(Variables.apply, Variables.unapply))
     *         )
     *
     *     val configOfList: ConfigDescriptor[List[Variables]] =
     *       lazyCollectAll(listOfConfig.head, listOfConfig.tail: _*)
     *
     *     val map =
     *       Map(
     *         "GROUP1_VARIABLE1" -> "1",
     *         "GROUP1_VARIABLE2" -> "2",
     *         "GROUP2_VARIABLE1" -> "3",
     *         "GROUP2_VARIABLE2" -> "4",
     *         "GROUP3_VARIABLE1" -> "5",
     *         "GROUP3_VARIABLE2" -> "6",
     *         "GROUP4_VARIABLE1" -> "7"
     *       )
     *
     *     // loadOrThrow here is only for the purpose of example
     *     val result: List[Variables] = read(configOfList from ConfigSource.fromMap(map, "constant")).loadOrThrow
     *
     *     val written: PropertyTree[String, String] = write(configOfList, result).loadOrThrow
     *
     *     assert(
     *       result == List(Variables(1, Some(2)), Variables(3, Some(4)), Variables(5, Some(6)), Variables(7, None))
     *     )
     * }}}
     */
    def lazyCollectAll[A](
      head: LazyConfigDescriptor[A],
      tail: LazyConfigDescriptor[A]*
    ): ConfigDescriptor[List[A]] =
      lazySequence(head, tail: _*)(
        { case (a, t) => a :: t },
        l => l.headOption.map(h => (h, l.tail))
      )

    /**
     *  `head` describes getting the head of a possible list value
     *
     *  Example:
     *
     *  {{{
     *     final case class Config(userName: String, port: Option[Int])
     *
     *   object Config {
     *     val source =
     *       ConfigSource.fromMap(Map("USERNAME" -> "af,sa", "PORT" -> "1"), valueDelimiter = Some(','))
     *     val databaseConfig: ConfigDescriptor[Config] =
     *       (head(string("USERNAME")) |@| int("PORT").optional)(Config.apply, Config.unapply)
     *   }
     *
     *   read(Config.databaseConfig from Config.source)
     *
     *   // returns Config("af", 1)
     *  }}}
     */
    def head[A](desc: ConfigDescriptor[A]): ConfigDescriptor[A] =
      desc.orElse(
        list(desc)
          .xmapEither[A](
            _.headOption
              .fold[Either[String, A]](Left("Element is missing"))(Right(_)),
            v => Right(v :: Nil)
          )
      )

    /**
     *  `head` describes getting the head of a possible list value
     *
     *  Example:
     *
     *  {{{
     *     final case class Config(userName: String, port: Option[Int])
     *
     *   object Config {
     *     val source =
     *       ConfigSource.fromMap(Map("USERNAME" -> "af,sa", "PORT" -> "1"), valueDelimiter = Some(','))
     *     val databaseConfig: ConfigDescriptor[Config] =
     *       (head("USERNAME")(string) |@| int("PORT").optional)(Config.apply, Config.unapply)
     *   }
     *
     *   read(Config.databaseConfig from Config.source)
     *
     *   // returns Config("af", 1)
     *  }}}
     */
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

    /**
     *
     * @param head
     * @param tail
     * @tparam A
     * @return
     */
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
