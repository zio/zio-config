package zio.config

import VersionSpecificSupport._
import scala.reflect.ClassTag

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
     * Instead the reverse relationship is `B => Option[A]`. `apply` is a "convenient" version of
     * `transformOrFail`, and is mainly used to `apply` and `unapply` case-classes from elements.
     *
     * Let's define a simple `ConfigDescriptor`,
     * that talks about retrieving a `String` configuration (example: PORT).
     *
     *  {{{
     *    val port: ConfigDescriptor[Int] = int("PORT")
     *  }}}
     *
     * Later you decided to convert the type of from `Int` to `String` (for some reason)
     *
     * In this case. In this case, `A => B` is `Int => String` which will always work.
     *
     * However, the reverse relationship (which is used to retrieve
     * the original type from its transformed representation) which is `String => Int` is no more a
     * total function, unless it was `String => Either[error, Int]` or `String => Option[Int]`.
     *
     * That is, not all elements of set `String` can be converted to `Int`.
     *
     * To overcome this, we can do `s => Try(s.toInt).toOption` to mark the possibility of errors.
     * This is a function of the type: `B => Option[A]`.
     *
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
     *  Write back the config:
     *
     *  Now given a stringified port "8888", you can also write it
     *  back to the source safely. It is safe write, and it is guaranteed that we can
     *  read back this config successfully. This is because, during writing back, it
     *  ensures that it is a real port that can be converted to `Int`.
     *
     *  {{{
     *
     *     import zio.config.typesafe._
     *      // NOTE: toJson is available only through zio-config-typesafe module (import zio.config.typesafe._)
     *
     *     val writtenBack: Either[String, PropertyTree[String, String]] =
     *       write(portString, "8888")
     *
     *     val jsonRepr: Either[String, String] =
     *       writtenBack.map(_.toJson) // { "port" : "8888" }
     *
     *     val mapRepr: Either[String, Map[String, String]] =
     *       writtenBack.map(_.flattenString()) // Map("port" -> "8888")
     *
     *      // And even this will work
     *      import zio.config.typesafe._
     *
     *      val port: Int = 8080
     *      port.toJson(portString)
     *
     *      // { "port" : "8080"  }
     *  }}}
     */
    def apply[B](app: A => B, unapp: B => Option[A]): ConfigDescriptor[B] =
      ConfigDescriptorAdt.transformOrFail(
        this,
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
    ): ProductBuilder[ConfigDescriptor, ConfigDescriptor, A, B] =
      new ProductBuilder[ConfigDescriptor, ConfigDescriptor, A, B] {

        override def zip[X, Y]: (ConfigDescriptor[X], ConfigDescriptor[Y]) => ConfigDescriptor[(X, Y)] =
          (a, b) => lazyDesc(a.zip(b))

        override def xmapEither[X, Y]
          : (ConfigDescriptor[X], X => Either[String, Y], Y => Either[String, X]) => ConfigDescriptor[Y] =
          (a, b, c) => lazyDesc(a.transformOrFail(b, c))

        override def pack[X](x: => ConfigDescriptor[X]): ConfigDescriptor[X] =
          lazyDesc(x)
        override def unpack[X](x: ConfigDescriptor[X]): ConfigDescriptor[X] = x

        override val a: ConfigDescriptor[A] = lazyDesc(self)
        override val b: ConfigDescriptor[B] = lazyDesc(that)
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

    /**
     * `default` function allows us to inject default values to existing config
     *
     * Example:
     *
     *  {{{ val port = int("PORT").default(8080) }}}
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
     *           (string("USERNAME") |@| int("PORT").default(8080))(Config.apply, Config.unapply)
     *     }
     *  }}}
     *
     *  In the above case, if username is missing, then it prints out an error, however if PORT is missing, it falls back to 8080.
     *
     *  In fact you can give a default to an entire config
     *
     *  For example:
     *
     *  {{{
     *     final case class Config(userName: String, port: Int)
     *
     *    object Config {
     *       val dbConfig: ConfigDescriptor[Config] =
     *          (string("USERNAME") |@| int("PORT"))(Config.apply, Config.unapply).default(Config("jon", 8080))
     *    }
     *
     *  }}}
     *
     *  Sometimes this can be used along with automatic derivation supported through zio-config-magnolia.
     *
     *  {{{
     *
     *     import zio.config.magnolia._, zio.config._, ConfigDescriptor._
     *
     *     final case class Config(userName: String, port: Int)
     *
     *     object Config {
     *       val dbConfig: ConfigDescriptor[Config] =
     *         descriptor[Config].default(Config("jon", 8080))
     *     }
     *
     *     // This is a typical example where we mix auto derivation with manual definitions.
     *
     *  }}}
     *
     */
    final def default(value: A): ConfigDescriptor[A] =
      ConfigDescriptorAdt.default(self, value) ?? s"default value: $value"

    /**
     * `describe` function allows us to inject additional documentation to the configuration parameters.
     *
     * Example:
     *
     *  {{{ val port = int("PORT") ?? "database port" }}}
     *
     * A more detailed example:
     *
     * Here is a program that describes (or a ConfigDescriptor that represents) reading a `USERNAME` which is a String and a `PORT` which is an Int,
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
      ConfigDescriptorAdt.describe(self, description)

    /**
     * Attach a source to the `ConfigDescriptor`.
     *
     * Example: {{{ val config = string("PORT") from ConfigSource.fromMap(Map.empty) }}}
     *
     * `config` is a description that says there is a key called `PORT` in constant map source.
     * You can use the description to read the config
     *
     * {{{
     *
     *   val either: Either[ReadError[String], String] = read(config)
     *
     * }}}
     *
     * You can also tag a source per config field, or one global source to an entire config.
     *
     * {{{
     *    final case class Config(userName: String, port: Int)
     *    object Config {
     *       val dbConfig: ConfigDescriptor[Config] =
     *          (string("USERNAME") |@| int("PORT"))(Config.apply, Config.unapply)
     *    }
     * }}}
     *
     * In the above example, `dbConfig` is not associated with any source. By default the source will be empty.
     *
     * To attach a config (especially during a read operation) is as easy as:
     *
     * {{{
     *   read(dbConfig from ConfigSource.fromMap(Map("USERNAME" -> "afs", "PORT" -> "8080"))
     *   // Right(Config("afs", 8080))
     * }}}
     *
     * Obviously, source can be attached independently.
     *
     * {{{
     *
     *   val configSource1: ConfigSource = ???
     *   val configSource2: ConfigSource = ???
     *
     *   val dbConfig =
     *     (string("USERNAME") from configSource1 |@| int("PORT"))(Config.apply, Config.unapply) from configSource2
     * }}}
     *
     * In the above case `read(dbConfig)` implies, zio-config tries to fetch `USERNAME` from configSource1, and if it
     * fails (i.e, missing value) it goes and try with the global config which is `configSource2`.
     * PORT will be fetched from configSource2.
     *
     * You can also try various sources for each field.
     *
     * {{{
     *
     *   val configSource1: ConfigSource = ??? // Example: ConfigSource.fromMap(...)
     *   val configSource2: ConfigSource = ??? // Example: ConfigSource.fromTypesafeConfig(...)
     *
     *   val dbConfig =
     *     (string("USERNAME") from configSource1.orElse(configSource2) |@|
     *       int("PORT") from configSource2.orElse(configSource1))(Config.apply, Config.unapply) from configSource2
     * }}}
     *
     *
     */
    final def from(that: ConfigSource): ConfigDescriptor[A] =
      self.updateSource(_.orElse(that))

    /**
     *
     * mapKey allows user to convert the keys in a ConfigDescriptor.
     *
     * Example:
     *
     * Consider you have a config that looks like this
     *
     * {{{
     *
     *   case class Config(url: String, port: Int)
     *
     *   object Config {
     *
     *      val config = (string("dbUrl") |@| int("dbPort"))(Config.apply, Config.unapply)
     *   }
     *
     *  val source = Map(
     *     "DB_URL" -> "abc.com",
     *     "DB_PORT" -> "9090"
     *  )
     *
     *  read(Config.config from ConfigSource.fromMap(source))
     *  // will fail since the source doesn't have the keys dbUrl and dbPort, but it has only DB_URL and DB_PORT
     *
     * }}}
     *
     * The above config retrieval fails since the keys dbUrl and dbPOrt exist, but it has only DB_URL and DB_PORT.
     * In this situation, instead of rewriting the config we can do
     *
     *
     * {{{
     *
     *   import zio.config._, ConfigDescriptor._
     *
     *   read(Config.config.mapKey(key => toSnakeCase(key).toUpperCase) from ConfigSource.fromMap(source))
     *   // Right(Config("abc.com", 9090))
     *
     * }}}
     */
    def mapKey(f: K => K): ConfigDescriptor[A] = {
      def loop[B](config: ConfigDescriptor[B]): ConfigDescriptor[B] =
        config match {
          case Lazy(thunk)                  => Lazy(() => loop(thunk()))
          case Source(source, propertyType) => Source(source, propertyType)
          case DynamicMap(source, conf)     => DynamicMap(source, loop(conf))
          case Nested(source, path, conf) =>
            Nested(source, f(path), loop(conf))
          case Optional(conf)          => Optional(loop(conf))
          case Sequence(source, conf)  => Sequence(source, loop(conf))
          case Describe(conf, message) => Describe(loop(conf), message)
          case Default(conf, value)    => Default(loop(conf), value)
          case TransformOrFail(conf, f, g) =>
            TransformOrFail(loop(conf), f, g)
          case Zip(conf1, conf2) => Zip(loop(conf1), loop(conf2))
          case OrElseEither(conf1, conf2) =>
            OrElseEither(loop(conf1), loop(conf2))
          case OrElse(value1, value2) =>
            OrElse(loop(value1), loop(value2))
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
      ConfigDescriptorAdt.optional(self) ?? "optional value"

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
      ConfigDescriptorAdt.orElse(self, that)

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
      ConfigDescriptorAdt.orElseEither(self, that)

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
          case Lazy(thunk)                  => Lazy(() => loop(thunk()))
          case Source(source, propertyType) => Source(f(source), propertyType)
          case DynamicMap(source, conf)     => DynamicMap(f(source), loop(conf))
          case Nested(source, path, conf) =>
            Nested(f(source), path, loop(conf))
          case Optional(conf)              => Optional(loop(conf))
          case Sequence(source, conf)      => Sequence(f(source), loop(conf))
          case Describe(conf, message)     => Describe(loop(conf), message)
          case Default(value, value2)      => Default(loop(value), value2)
          case TransformOrFail(conf, f, g) => TransformOrFail(loop(conf), f, g)
          case Zip(conf1, conf2)           => Zip(loop(conf1), loop(conf2))
          case OrElseEither(value1, value2) =>
            OrElseEither(loop(value1), loop(value2))
          case OrElse(value1, value2) =>
            OrElse(loop(value1), loop(value2))
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
      self.transformOrFail(a => Right(to(a)), b => Right(from(b)))

    /**
     * Given `A` and `B`, `transformOrFail` function is used to convert a `ConfigDescriptor[A]` to `ConfigDescriptor[B]`.
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
    final def transformOrFail[B](
      to: A => Either[String, B],
      from: B => Either[String, A]
    ): ConfigDescriptor[B] =
      ConfigDescriptorAdt.transformOrFail(self, to, from)

    final def transformOrFailLeft[B](f: A => Either[String, B])(g: B => A): ConfigDescriptor[B] =
      self.transformOrFail(f, b => Right(g(b)))

    final def transformOrFailRight[B](
      f: A => B,
      g: B => Either[String, A]
    ): ConfigDescriptor[B] =
      self.transformOrFail(t => Right(f(t)), (g))

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
      ConfigDescriptorAdt.zip(self, that)

    /**
     * `zipWith` is similar to `xmapEither` but the function
     * is mostly used as an internal implementation
     * in zio-config. For the same reason, users hardly need `zipWith`.
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
    final def zipWith[B, C](that: => ConfigDescriptor[B])(
      to: (A, B) => Either[String, C],
      from: C => Either[String, (A, B)]
    ): ConfigDescriptor[C] =
      (self |@| that)
        .apply[(A, B)](Tuple2.apply, Tuple2.unapply)
        .transformOrFail({ case (a, b) => to(a, b) }, from)
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
     *             (int(s"\${group}_VARIABLE1") |@| int(s"\${group}_VARIABLE2").optional)(Variables.apply, Variables.unapply)
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
    def collectAll[A](head: => ConfigDescriptor[A], tail: ConfigDescriptor[A]*): ConfigDescriptor[List[A]] =
      tail
        .map(lazyDesc(_))
        .reverse
        .foldLeft[ConfigDescriptor[(A, List[A])]](
          lazyDesc(head)
            .transform((a: A) => (a, Nil), (b: (A, List[A])) => b._1)
        )(
          (b: ConfigDescriptor[(A, List[A])], a: ConfigDescriptor[A]) =>
            b.zipWith(a)({
              case ((first, tail), a) => Right((first, a :: tail))
            }, {
              case (_, Nil)              => Left("Invalid list length")
              case (first, head :: tail) => Right(((first, tail), head))
            })
        )({ case (a, t) => a :: t }, l => l.headOption.map(h => (h, l.tail)))

    /**
     * enumeration allows user to up-cast all the subtypes to its super type defined by `D`.
     * This is mainly useful in defining `coproducts` (`sealed trait`)
     *
     * Example:
     * {{{
     *   sealed trait D
     *
     *   case class A(a: String) extends D
     *   case class B(b: Int) extends D
     *   case class C(c: Double) extends D
     *
     *   val config: ConfigDescriptor[D] =
     *     enumeration[D](
     *       string("a")(A.Apply, A.unapply),
     *       int("b")(B.apply, B.unapply),
     *       double("c")(C.apply, C.unapply)
     *     )
     * }}}
     *
     * Currently enumeration supports to a maximum of 9 terms. If you have more terms, use `orElse`
     * to combine the terms.
     *
     * {{{
     *    enumeration[D](a, b, c, d, e, f, g, h) orElse enumeration[D](i, j, k)
     * }}}
     *
     * NOTE:
     *
     * Use zio-config-magnolia for better compile time safety when it comes to `sealed trait`,
     * as it has strong compile time behaviour and makes sure all subtypes are being handled.
     * On the other hand, `enumeration` doesn't complain at compile time if you forgot
     * to pass the config descriptor of any of the subtype.
     *
     * Example:
     *
     * {{{
     *   import zio.config.magnolia._
     *
     *   val config = descriptor[D]
     * }}}
     */
    def enumeration[D] = new PartiallyAppliedEnumeration[D]

    class PartiallyAppliedEnumeration[D] {
      def apply[X <: D](
        desc1: ConfigDescriptor[X]
      )(implicit tag: ClassTag[X]): ConfigDescriptor[D] =
        desc1.transformOrFail(
          (a: X) => Right(a: D),
          (d: D) =>
            d match {
              case a: X => Right(a)
              case _ =>
                Left(
                  s"""
                  "Cannot write the config back because instance type doesn't match.
                  This can also happen if ConfigDescriptor is not aware of a particular subtype.
                  Make sure all subtypes (or the type being written back) has been passed to enumeration while creating ConfigDescriptor.
                  Or use auto derivation in zio-config-magnolia for better static/compile-time safety if its a sealed-trait"
                  """
                )
            }
        )

      def apply[A <: D: ClassTag, B <: D: ClassTag](
        desc1: ConfigDescriptor[A],
        desc2: ConfigDescriptor[B]
      ): ConfigDescriptor[D] =
        apply(desc1) orElse apply(desc2)

      def apply[A <: D: ClassTag, B <: D: ClassTag, C <: D: ClassTag](
        desc1: ConfigDescriptor[A],
        desc2: ConfigDescriptor[B],
        desc3: ConfigDescriptor[C]
      ): ConfigDescriptor[D] =
        apply(desc1, desc2).orElse(apply[C](desc3))

      def apply[A <: D: ClassTag, B <: D: ClassTag, C <: D: ClassTag, E <: D: ClassTag](
        desc1: ConfigDescriptor[A],
        desc2: ConfigDescriptor[B],
        desc3: ConfigDescriptor[C],
        desc4: ConfigDescriptor[E]
      ): ConfigDescriptor[D] =
        apply(desc1, desc2, desc3) orElse apply[E](desc4)

      def apply[A <: D: ClassTag, B <: D: ClassTag, C <: D: ClassTag, E <: D: ClassTag, F <: D: ClassTag](
        desc1: ConfigDescriptor[A],
        desc2: ConfigDescriptor[B],
        desc3: ConfigDescriptor[C],
        desc4: ConfigDescriptor[E],
        desc5: ConfigDescriptor[F]
      ): ConfigDescriptor[D] =
        apply(desc1, desc2, desc3, desc4) orElse apply(desc5)

      def apply[
        A <: D: ClassTag,
        B <: D: ClassTag,
        C <: D: ClassTag,
        E <: D: ClassTag,
        F <: D: ClassTag,
        G <: D: ClassTag
      ](
        desc1: ConfigDescriptor[A],
        desc2: ConfigDescriptor[B],
        desc3: ConfigDescriptor[C],
        desc4: ConfigDescriptor[E],
        desc5: ConfigDescriptor[F],
        desc6: ConfigDescriptor[G]
      ): ConfigDescriptor[D] =
        apply(desc1, desc2, desc3, desc4, desc5) orElse apply(desc6)

      def apply[
        A <: D: ClassTag,
        B <: D: ClassTag,
        C <: D: ClassTag,
        E <: D: ClassTag,
        F <: D: ClassTag,
        G <: D: ClassTag,
        H <: D: ClassTag
      ](
        desc1: ConfigDescriptor[A],
        desc2: ConfigDescriptor[B],
        desc3: ConfigDescriptor[C],
        desc4: ConfigDescriptor[E],
        desc5: ConfigDescriptor[F],
        desc6: ConfigDescriptor[G],
        desc7: ConfigDescriptor[H]
      ): ConfigDescriptor[D] =
        apply(desc1, desc2, desc3, desc4, desc5, desc6) orElse apply(desc7)

      def apply[
        A <: D: ClassTag,
        B <: D: ClassTag,
        C <: D: ClassTag,
        E <: D: ClassTag,
        F <: D: ClassTag,
        G <: D: ClassTag,
        H <: D: ClassTag,
        I <: D: ClassTag
      ](
        desc1: ConfigDescriptor[A],
        desc2: ConfigDescriptor[B],
        desc3: ConfigDescriptor[C],
        desc4: ConfigDescriptor[E],
        desc5: ConfigDescriptor[F],
        desc6: ConfigDescriptor[G],
        desc7: ConfigDescriptor[H],
        desc8: ConfigDescriptor[I]
      ): ConfigDescriptor[D] =
        apply(desc1, desc2, desc3, desc4, desc5, desc6, desc7) orElse apply(desc8)

      def apply[
        A <: D: ClassTag,
        B <: D: ClassTag,
        C <: D: ClassTag,
        E <: D: ClassTag,
        F <: D: ClassTag,
        G <: D: ClassTag,
        H <: D: ClassTag,
        I <: D: ClassTag,
        J <: D: ClassTag
      ](
        desc1: ConfigDescriptor[A],
        desc2: ConfigDescriptor[B],
        desc3: ConfigDescriptor[C],
        desc4: ConfigDescriptor[E],
        desc5: ConfigDescriptor[F],
        desc6: ConfigDescriptor[G],
        desc7: ConfigDescriptor[H],
        desc8: ConfigDescriptor[I],
        desc9: ConfigDescriptor[J]
      ): ConfigDescriptor[D] =
        apply(desc1, desc2, desc3, desc4, desc5, desc6, desc7, desc8) orElse apply(desc9)
    }

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
          .transformOrFail[A](
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

    /**
     *  `list(confgDescriptor)` represents just a list variant of configuration extraction.
     *
     *  For example, we know `val config = string("USERNAME") from source`
     *  represents a program that says, there exists a key called
     *  "USERNAME" (in some ConfigSource called source)
     *  with a value that is of the type `String`.
     *
     *  `list(config)` would then imply, there exists a list of `USERNAME -> value` pair.
     *
     *  Given below is a complete example:
     *
     *  {{{
     *    val json =
     *       s"""
     *          | xyz : [
     *          |   {
     *          |     "USERNAME" : "value1"
     *          |   },
     *          |
     *          |   {
     *          |     "USERNAME" : "value2"
     *          |   }
     *          | ]
     *          |""".stripMargin
     *
     *     val getSource: Either[ReadError[String], ConfigSource] =
     *       TypesafeConfigSource.fromHoconString(json)
     *
     *     val config = string("USERNAME")
     *
     *     // Within the key "xyz", we have a list of key-value pair, where key is always "USERNAME"
     *     // NOTE: In HOCON, there is always a need of key (in this case, xyz) at parent level.
     *
     *     val listConfig = nested("xyz")(list(config))
     *
     *     val userNames: Either[ReadError[String], List[String]] =
     *       getSource.flatMap(source => read(listConfig from source))
     *
     *     println(userNames)
     *  }}}
     *
     *  returns
     *
     *  {{{
     *
     *    Right(List(value1, value2))
     *
     *  }}}
     *
     *  NOTE:
     *
     *  `nested("xyz")(list(string("USERNAME"))` is same as `list("xyz")(string("USERNAME"))`
     */
    def list[K, V, A](desc: => ConfigDescriptor[A]): ConfigDescriptor[List[A]] =
      ConfigDescriptorAdt.sequence(ConfigSourceFunctions.empty, desc)

    /**
     *  `list("xyz")(confgDescriptor)` represents just a list variant of configDescriptor within the key `xyz`.
     *  Note that, `nested("xyz")(list(configDescriptor))` is same as `list("xyz")(configDescriptor)`.
     *
     *  For example: `list("key")(string)` implies value of `key` is of the type `List[String]`
     *
     *  Here is a more detailed example.
     *
     *  We know `val config = string("USERNAME") from source`
     *  represents a program that says, there exists a key called
     *  "USERNAME" (in some ConfigSource called source)
     *  with a value that is of the type `String`.
     *
     *  `list("xyz")(config)` would then imply, there exists a list of `USERNAME -> value` pair within the key "xyz".
     *
     *  {{{
     *    val json =
     *       s"""
     *          | xyz : [
     *          |   {
     *          |     "USERNAME" : "value1"
     *          |   },
     *          |
     *          |   {
     *          |     "USERNAME" : "value2"
     *          |   }
     *          | ]
     *          |""".stripMargin
     *
     *     val getSource: Either[ReadError[String], ConfigSource] =
     *       TypesafeConfigSource.fromHoconString(json)
     *
     *     val config = string("USERNAME")
     *
     *     // Within the key "xyz", we have a list of key-value pair, where key is always "USERNAME"
     *     // NOTE: In HOCON, there is always a need of key (in this case, xyz) at parent level.
     *
     *     val listConfig = list("xyz")(config)
     *
     *     val userNames: Either[ReadError[String], List[String]] =
     *       getSource.flatMap(source => read(listConfig from source))
     *
     *     println(userNames)
     *  }}}
     *
     *  returns
     *
     *  {{{
     *
     *    Right(List(value1, value2))
     *
     *  }}}
     */
    def list[A](
      path: K
    )(desc: => ConfigDescriptor[A]): ConfigDescriptor[List[A]] =
      nested(path)(list(desc))

    /**
     *  `listOrSingleton` is a flexible version of `list`. This means, even if the value is not of the type `List`
     *  it considers the value a singleton and returns `List(singleValue)`
     *
     *  We `list("xyz")(confgDescriptor)` represents just a list variant of configDescriptor within the key `xyz`.
     *  That is `list("key")(string)` implies value of `key` is of the type `List[String]`
     *
     *  However if the value of `key` was not a list, but instead a simple string, and if we are using `listOrSingleton`
     *  it will be considered as a `List`.
     *
     *  Here is a more detailed example.
     *
     *  {{{
     *      val json =
     *       s"""
     *          | USERNAME : {
     *          |     "USERNAME" : "abc"
     *          |   }
     *          |""".stripMargin
     *
     *     val getSource: Either[ReadError[String], ConfigSource] =
     *       TypesafeConfigSource.fromHoconString(json)
     *
     *     val config = string("USERNAME")
     *
     *     val usernames: Either[ReadError[String], List[String]] =
     *       getSource.flatMap(
     *         source => read(listOrSingleton("configs")(config) from source)
     *       )
     *  }}}
     *
     *  returns
     *
     *  {{{
     *
     *    Right(List(value1))
     *
     *  }}}
     */
    def listOrSingleton[A](
      path: K
    )(desc: => ConfigDescriptor[A]): ConfigDescriptor[List[A]] =
      list(path)(desc) orElse (
        nested(path)(desc)
          .transformOrFail[List[A]](
            value => Right(List(value)),
            _.headOption match {
              case Some(value) => Right(value)
              case None        => Left("Cannot write an empty list back")
            }
          )
        )

    /**
     * Retrieve a `Map`given an existing `ConfigDescriptor`.
     *
     * `map(configDescriptor)` is similar to `map(path)(configDescriptor)` except
     * that there is no `path` associated with it. For the same reason, you would need
     * the second version given below:  `def map[A](path: K)(desc: => ConfigDescriptor[A])
     *
     * Before we try to understand the semantics of `map(configDescriptor)`, let's understand the
     * semantics of `map(path)(configDescriptor)`; a function with the same name given below,
     * but it takes a path as well.
     *
     *  `map("xyz")(confgDescriptor)` represents retrieving a map (of key value pairs) that exists within the key "xyz"
     *
     *  Let's explain this in detail with an example: int("URL") implies there exists a value of the type string under the key "URL"
     *  On similar lines, map("URL")(int) implies there exists a value of the type `Map` under the key `URL` and the type of the
     *  value of each key in the map is of the type Int.
     *
     *  Sidee note: Obviously, for complex types such as Map,
     *  you can also rely on zio-config-magnolia that allows you to retrieve
     *  any value of the type Map[String, A] for all type A,
     *  that has an instance of `Description` (refer zio-config-magnolia api docs)
     *
     *  {{{
     *
     *    val config = map("xyz")(int)
     *
     *    val sourceOrFailed: Either[ReadError[String], ConfigSource] =
     *      TypesafeConfigSource.fromHoconString(
     *        "xyz" : {
     *           "key1" : "1"
     *           "key2" : "2"
     *           "key3" : "3"
     *         }
     *      )
     *
     *    // Forming a TypesafeConfigSource from string returned an Either (being able to capture errors) because
     *    // the HOCON string can be an invalid string.
     *
     *    val result =  sourceOrFailed.flatMap(source => read(config from source))
     *    // Right(Map("key1" -> 1, "key2" -> 2, "key3" -> 3))
     *  }}}
     *
     *  We explained `map` using TypesafeConfigSource. However, for zio-config source doesn't really matter.
     *  For example, lets try to fetch a map from a flattened scala Map.
     *
     *  {{{
     *     val source = ConfigSource.fromMap(
     *        Map(
     *          "xyz_key1" -> "1",
     *          "xyz_key2" -> "2",
     *          "xyz_key3" -> "3"
     *        ), keyDelimiter = Some('_')
     *     )
     *
     *    val config = read(config from source)
     *    // Right( Map("key1" -> 1, "key2" -> 2, "key3" -> 3))
     *
     *  }}}
     *
     *
     * Now what does it mean if we say ` val config = map(int("id")) ` instead of `val config = map("id")(int)`
     *
     * The difference is `map("id")(int)` implies there exists a map within the key `id`, whose values of are of the type `Int`
     * On the other hand `map(int("id"))` implies there exists a map hose value is of the type  {"id" : "Int"}
     *
     *
     * Example:
     *  {{{
     *
     *   val mapConfig = map(int("id"))
     *
     *   // This means there exists a Map whose value is of the type {"String" : "Int"}.
     *
     *   val sourceOrFailure: Either[ReadError[String], TypesafeConfigSource] = TypesafeConfigSource.fromHoconString(
     *     s"""
     *      "abc" : {
     *       "key1" : { "id" :  "2" },
     *       "key2" : { "id" : "3" }
     *     }
     *
     *      """"
     *    )
     *
     *   val result = sourceOrFailure.flatMap(s => read(nested("abc")(map(int("id"))) from s))
     *   // Right(Map("key1" -> 1, "key2" -> 2))
     *
     *  }}}
     *
     * This is really useful when the config source consist of a map but you need to fetch the value of the keys
     * in the map from an nested key within itself. In this example it is "id".

     */
    def map[A](desc: => ConfigDescriptor[A]): ConfigDescriptor[Map[K, A]] =
      ConfigDescriptorAdt.dynamicMap(ConfigSourceFunctions.empty, desc)

    /**
     *  `map("xyz")(confgDescriptor)` represents retrieving a map (of key value pairs) that exists within the key "xyz"
     *
     *  Let's explain this in detail with an example: int("URL") implies there exists a value of the type string under the key "URL"
     *  On similar lines, map("URL")(int) implies there exists a value of the type `Map` under the key `URL` and the type of the
     *  value of each key in the map is of the type Int.
     *
     *  Sidee note: Obviously, for complex types such as Map,
     *  you can also rely on zio-config-magnolia that allows you to retrieve
     *  any value of the type Map[String, A] for all type A,
     *  that has an instance of `Description` (refer zio-config-magnolia api docs)
     *
     *  {{{
     *
     *    val config = map("xyz")(int)
     *
     *    val sourceOrFailed: Either[ReadError[String], ConfigSource] =
     *      TypesafeConfigSource.fromHoconString(
     *        "xyz" : {
     *           "key1" : "1"
     *           "key2" : "2"
     *           "key3" : "3"
     *         }
     *      )
     *
     *    // Forming a TypesafeConfigSource from string returned an Either (being able to capture errors) because
     *    // the HOCON string can be an invalid string.
     *
     *    val result =  sourceOrFailed.flatMap(source => read(config from source))
     *    // Right(Map("key1" -> 1, "key2" -> 2, "key3" -> 3))
     *  }}}
     *
     *  We explained `map` using TypesafeConfigSource. However, for zio-config source doesn't really matter.
     *  For example, lets try to fetch a map from a flattened scala Map.
     *
     *  {{{
     *     val source = ConfigSource.fromMap(
     *        Map(
     *          "xyz_key1" -> "1",
     *          "xyz_key2" -> "2",
     *          "xyz_key3" -> "3"
     *        ), keyDelimiter = Some('_')
     *     )
     *
     *    val config = read(config from source)
     *    // Right( Map("key1" -> 1, "key2" -> 2, "key3" -> 3))
     *
     *  }}}
     */
    def map[A](
      path: K
    )(desc: => ConfigDescriptor[A]): ConfigDescriptor[Map[K, A]] =
      nested(path)(map(desc))

    /**
     * nested allows us to retrieve a config from a path `K`, where `K` is typically `String`.
     *
     * Example :
     *
     * {{{
     *    val config = nested("key")(string)
     *    val mapSource = ConfigSource.fromMap(
     *       "key" : "value"
     *    )
     *
     *    val result = read(config from mapSource)
     *    // Right("value")
     * }}}
     *
     * Note that `string("key")` is same as that of `nested("key")(string)`
     */
    def nested[A](path: K)(desc: => ConfigDescriptor[A]): ConfigDescriptor[A] =
      ConfigDescriptorAdt.nested(ConfigSourceFunctions.empty, path, desc)

    /**
     *  `set("xyz")(confgDescriptor)` represents just a set variant of configDescriptor within the key `xyz`.
     *  Note that, `nested("xyz")(set(configDescriptor))` is same as `set("xyz")(configDescriptor)`.
     *
     *  For example: `set("key")(string)` implies value of `key` is of the type `Set[String]`
     *
     *  Here is a more detailed example.

     *  `list("xyz")(string)` would then imply, there exists a set of type String under "xyz"
     *
     *  {{{
     *    val json =
     *       s"""
     *          | xyz : ["a", "b"]
     *          |""".stripMargin
     *
     *    val sourceOrFailure: Either[ReadError[String], TypesafeConfigSource] =
     *     TypesafeConfigSource.fromHoconString(json)
     *
     *   sourceOrFailure.flatMap(source => read(set("xyz")(string) from source))
     *
     *  }}}
     *
     *  returns
     *
     *  {{{
     *
     *    Right(List(value1, value2))
     *
     *  }}}
     */
    def set[K, V, A](desc: => ConfigDescriptor[A]): ConfigDescriptor[Set[A]] =
      list(desc).transformOrFail(distinctListToSet, s => Right(s.toList))

    /**
     *  `set("xyz")(confgDescriptor)` represents just a set variant of configDescriptor within the key `xyz`.
     *  Note that, `nested("xyz")(set(configDescriptor))` is same as `set("xyz")(configDescriptor)`.
     *
     *  For example: `set("key")(string)` implies value of `key` is of the type `Set[String]`
     *
     *  Here is a more detailed example.
     *
     *  `list("xyz")(string)` would then imply, there exists a set of type String under "xyz"
     *
     *  {{{
     *    val json =
     *       s"""
     *          | xyz : ["a", "b"]
     *          |""".stripMargin
     *
     *    val sourceOrFailure: Either[ReadError[String], TypesafeConfigSource] =
     *     TypesafeConfigSource.fromHoconString(json)
     *
     *   sourceOrFailure.flatMap(source => read(set("xyz")(string) from source))
     *
     *  }}}
     *
     *  returns
     *
     *  {{{
     *
     *    Right(List(value1, value2))
     *
     *  }}}
     */
    def set[A](
      path: K
    )(desc: => ConfigDescriptor[A]): ConfigDescriptor[Set[A]] =
      nested(path)(set(desc))

    private[config] def distinctListToSet[A](list: List[A]): Either[String, Set[A]] =
      if (list.size == list.distinct.size) Right(list.toSet)
      else Left("Duplicated values found")

  }

  private[config] object ConfigDescriptorAdt {
    sealed case class Default[A](config: ConfigDescriptor[A], default: A) extends ConfigDescriptor[A]

    sealed case class Describe[A](config: ConfigDescriptor[A], message: String) extends ConfigDescriptor[A]

    sealed case class DynamicMap[A](source: ConfigSource, config: ConfigDescriptor[A])
        extends ConfigDescriptor[Map[K, A]]

    sealed case class Lazy[A](private val get: () => ConfigDescriptor[A]) extends ConfigDescriptor[A]

    sealed case class Nested[A](source: ConfigSource, path: K, config: ConfigDescriptor[A]) extends ConfigDescriptor[A]

    sealed case class Optional[A](config: ConfigDescriptor[A]) extends ConfigDescriptor[Option[A]]

    sealed case class OrElse[A](left: ConfigDescriptor[A], right: ConfigDescriptor[A]) extends ConfigDescriptor[A]

    sealed case class OrElseEither[A, B](left: ConfigDescriptor[A], right: ConfigDescriptor[B])
        extends ConfigDescriptor[Either[A, B]]

    sealed case class Sequence[A](source: ConfigSource, config: ConfigDescriptor[A]) extends ConfigDescriptor[List[A]]

    sealed case class Source[A](source: ConfigSource, propertyType: PropertyType[V, A]) extends ConfigDescriptor[A]

    sealed case class Zip[A, B](left: ConfigDescriptor[A], right: ConfigDescriptor[B]) extends ConfigDescriptor[(A, B)]

    sealed case class TransformOrFail[A, B](
      config: ConfigDescriptor[A],
      f: A => Either[String, B],
      g: B => Either[String, A]
    ) extends ConfigDescriptor[B]

    final def default[A](config: => ConfigDescriptor[A], default: A): ConfigDescriptor[A] =
      Default(lazyDesc(config), default)

    final def describe[A](config: => ConfigDescriptor[A], message: String): ConfigDescriptor[A] =
      Describe(lazyDesc(config), message)

    final def dynamicMap[A](source: ConfigSource, config: => ConfigDescriptor[A]): ConfigDescriptor[Map[K, A]] =
      DynamicMap(source, lazyDesc(config))

    final def lazyDesc[A](
      config: => ConfigDescriptor[A]
    ): ConfigDescriptor[A] = {
      lazy val config0 = config

      Lazy(() => config0)
    }

    final def nested[A](source: ConfigSource, path: K, config: => ConfigDescriptor[A]): ConfigDescriptor[A] =
      Nested(source, path, lazyDesc(config))

    final def optional[A](config: => ConfigDescriptor[A]): ConfigDescriptor[Option[A]] =
      Optional(lazyDesc(config))

    final def orElse[A](left: => ConfigDescriptor[A], right: => ConfigDescriptor[A]): ConfigDescriptor[A] =
      OrElse(lazyDesc(left), lazyDesc(right))

    final def orElseEither[A, B](
      left: => ConfigDescriptor[A],
      right: => ConfigDescriptor[B]
    ): ConfigDescriptor[Either[A, B]] =
      OrElseEither(lazyDesc(left), lazyDesc(right))

    final def sequence[A](source: ConfigSource, config: => ConfigDescriptor[A]): ConfigDescriptor[List[A]] =
      Sequence(source, lazyDesc(config))

    final def source[A](source: ConfigSource, propertyType: PropertyType[V, A]): ConfigDescriptor[A] =
      Source(source, propertyType)

    final def zip[A, B](left: => ConfigDescriptor[A], right: => ConfigDescriptor[B]): ConfigDescriptor[(A, B)] =
      Zip(lazyDesc(left), lazyDesc(right))

    final def transformOrFail[A, B](
      config: => ConfigDescriptor[A],
      f: A => Either[String, B],
      g: B => Either[String, A]
    ) =
      TransformOrFail(lazyDesc(config), f, g)
  }

}
