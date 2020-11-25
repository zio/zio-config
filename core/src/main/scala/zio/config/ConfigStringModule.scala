package zio.config

import java.io.File
import java.net.{ URI, URL }
import java.time.{ Instant, LocalDate, LocalDateTime, LocalTime }
import java.util.UUID
import java.util.Properties

import zio.{ Has, Layer, Tag, ZIO, ZLayer }
import zio.system.System

import scala.concurrent.duration.Duration

trait ConfigStringModule extends ConfigModule with ConfigSourceStringModule {

  object ConfigDescriptor extends ConfigDescriptorFunctions {
    import ConfigDescriptorAdt._

    /**
     * A config descriptor that describes retrieving a big-decimal.
     *
     * Note that there is no path associated with it. However, an example can give you more idea on what's going on.
     *
     * {{{
     *
     *     val valueConfig: ConfigDescriptor[Either[BigDecimal, String]] = bigDecimal.orElseEither(string)
     *
     *     // Describes fetching a map that is under the path "key-values" where the value of each can be either a BigDecimal or
     *     // if it's not try to fetch it as a String. An example source config
     *
     *     val sourceString =
     *       """
     *           {
     *               key-values : {
     *                  key1 : "usa"
     *                  key2 : "111111111111"
     *                  key3 : "australia"
     *               }
     *            }
     *        """
     *
     *     val hoconSource = TypesafeConfigSource.fromHoconString(sourceString)
     *
     *     val mapConfig = map("key-values")(valueConfig)
     *
     *     val getMapConfig: ConfigDescriptor[Map[String, Either[BigDecimal, String]] =
     *        hoconSource.flatMap(source => read(mapConfig from source)
     *
     * }}}
     *
     */
    val bigDecimal: ConfigDescriptor[BigDecimal] =
      source(ConfigSource.empty, PropertyType.BigDecimalType) ?? "value of type bigdecimal"

    /**
     * A config descriptor that describes retrieving a big-decimal from a given path.
     *
     * {{{
     *
     *     val mapSource =
     *      ConfigSource.fromMap(
     *         "COST" : 111111111
     *      )
     *
     *     val config = bigDecimal("COST")
     *     val result = read(config from mapSource)
     *
     *     // Right(111111111)
     *
     * }}}
     *
     */
    def bigDecimal(path: String): ConfigDescriptor[BigDecimal] = nested(path)(bigDecimal)

    /**
     * A config descriptor that describes retrieving a BigInt.
     *
     * Note that there is no path associated with it. However, an example can give you more idea on what's going on.
     *
     * {{{
     *
     *     val valueConfig: ConfigDescriptor[Either[BigInt, String]] = bigInt.orElseEither(string)
     *
     *     // Describes fetching a map that is under the path "key-values" where the value of each can be either a BigDecimal or
     *     // if it's not try to fetch it as a String. An example source config
     *
     *     val sourceString =
     *       """
     *           {
     *               key-values : {
     *                  key1 : "usa"
     *                  key2 : "111111111111"
     *                  key3 : "australia"
     *               }
     *            }
     *        """
     *
     *     val hoconSource = TypesafeConfigSource.fromHoconString(sourceString)
     *
     *     val mapConfig = map("key-values")(valueConfig)
     *
     *     val getMapConfig: ConfigDescriptor[Map[String, Either[BigInt, String]] =
     *        hoconSource.flatMap(source => read(mapConfig from source)
     *
     * }}}
     *
     */
    val bigInt: ConfigDescriptor[BigInt] =
      source(ConfigSource.empty, PropertyType.BigIntType) ?? "value of type bigint"

    /**
     * A config descriptor that describes retrieving a BigInt from a given path.
     *
     * {{{
     *
     *     val mapSource =
     *      ConfigSource.fromMap(
     *         "COST" : 111111111
     *      )
     *
     *     val config = bigInt("COST")
     *     val result = read(config from mapSource)
     *
     *     // Right(111111111)
     *
     * }}}
     *
     */
    def bigInt(path: String): ConfigDescriptor[BigInt] = nested(path)(bigInt)

    val boolean: ConfigDescriptor[Boolean] =
      source(ConfigSource.empty, PropertyType.BooleanType) ?? "value of type boolean"

    /**
     * A config descriptor that describes retrieving a Boolean from a given path.
     *
     * {{{
     *
     *     val mapSource =
     *      ConfigSource.fromMap(
     *         "IS_TRUE" : true
     *      )
     *
     *     val config = boolean("IS_TRUE")
     *     val result = read(config from mapSource)
     *
     *     // Right(true)
     *
     * }}}
     *
     */
    def boolean(path: String): ConfigDescriptor[Boolean] = nested(path)(boolean)

    val byte: ConfigDescriptor[Byte] =
      source(ConfigSource.empty, PropertyType.ByteType) ?? "value of type byte"

    /**
     * A config descriptor that describes retrieving a Byte from a given path.
     *
     * {{{
     *
     *     val mapSource =
     *      ConfigSource.fromMap(
     *         "KEY" : 11
     *      )
     *
     *     val config = byte("KEY")
     *     val result = read(config from mapSource)
     *
     *     // Right(11)
     *
     * }}}
     *
     */
    def byte(path: String): ConfigDescriptor[Byte] = nested(path)(byte)

    val double: ConfigDescriptor[Double] =
      source(ConfigSource.empty, PropertyType.DoubleType) ?? "value of type double"

    /**
     * A config descriptor that describes retrieving a Double from a given path.
     *
     * {{{
     *
     *     val mapSource =
     *      ConfigSource.fromMap(
     *         "COST" : 11.11
     *      )
     *
     *     val config = double("COST")
     *     val result = read(config from mapSource)
     *
     *     // Right(11.11)
     *
     * }}}
     *
     */
    def double(path: String): ConfigDescriptor[Double] = nested(path)(double)

    val duration: ConfigDescriptor[Duration] =
      source(ConfigSource.empty, PropertyType.DurationType) ?? "value of type duration"

    /**
     * A config descriptor that describes retrieving a duration from a given path.
     *
     * {{{
     *
     *     val mapSource =
     *      ConfigSource.fromMap(
     *         "DURATION" : "3 seconds"
     *      )
     *
     *     val config = duration("DURATION")
     *     val result = read(config from mapSource)
     *
     *     // Right(3 seconds)
     *
     * }}}
     *
     */
    def duration(path: String): ConfigDescriptor[Duration] = nested(path)(duration)

    val file: ConfigDescriptor[File] =
      source(ConfigSource.empty, PropertyType.FileType) ?? "value of type file"

    /**
     * A config descriptor that describes retrieving a file from a given path.
     *
     * {{{
     *
     *     val mapSource =
     *      ConfigSource.fromMap(
     *         "FILE_PATH" : "/user/file.txt"
     *      )
     *
     *     val config = file("FILE_PATH")
     *     val result = read(config from mapSource)
     *
     *     // Right(/user/file.txt)
     *
     * }}}
     *
     */
    def file(path: String): ConfigDescriptor[File] = nested(path)(file)

    val float: ConfigDescriptor[Float] =
      source(ConfigSource.empty, PropertyType.FloatType) ?? "value of type float"

    /**
     * A config descriptor that describes retrieving a Float from a given path.
     *
     * {{{
     *
     *     val mapSource =
     *      ConfigSource.fromMap(
     *         "COST" : 1.2
     *      )
     *
     *     val config = float("COST")
     *     val result = read(config from mapSource)
     *
     *     // Right(1.2f)
     *
     * }}}
     *
     */
    def float(path: String): ConfigDescriptor[Float] = nested(path)(float)

    val instant: ConfigDescriptor[Instant] =
      source(ConfigSource.empty, PropertyType.InstantType) ?? "value of type instant"

    /**
     * A config descriptor that describes retrieving a Instant from a given path.
     *
     * {{{
     *
     *     val mapSource =
     *      ConfigSource.fromMap(
     *         "TIME" : 2020-11-24T23:21:33.034557Z
     *      )
     *
     *     val config = instant("TIME")
     *     val result = read(config from mapSource)
     *
     *     // Right( 2020-11-24T23:21:33.034557Z)
     *
     * }}}
     *
     */
    def instant(path: String): ConfigDescriptor[Instant] = nested(path)(instant)

    val int: ConfigDescriptor[Int] =
      source(ConfigSource.empty, PropertyType.IntType) ?? "value of type int"

    /**
     * A config descriptor that describes retrieving a Int from a given path.
     *
     * {{{
     *
     *     val mapSource =
     *      ConfigSource.fromMap(
     *         "COST" : 10
     *      )
     *
     *     val config = int("COST")
     *     val result = read(config from mapSource)
     *
     *     // Right(10)
     *
     * }}}
     *
     */
    def int(path: String): ConfigDescriptor[Int] = nested(path)(int)

    val localDate: ConfigDescriptor[LocalDate] =
      source(ConfigSource.empty, PropertyType.LocalDateType) ?? "value of type localdate"

    /**
     * A config descriptor that describes retrieving a LocalDate from a given path.
     *
     * {{{
     *
     *     val mapSource =
     *      ConfigSource.fromMap(
     *         "date" : "2020-01-01"
     *      )
     *
     *     val config = localDate("date")
     *     val result = read(config from mapSource)
     *
     *     // Right(2020-01-01)
     *
     * }}}
     *
     */
    def localDate(path: String): ConfigDescriptor[LocalDate] = nested(path)(localDate)

    val localDateTime: ConfigDescriptor[LocalDateTime] =
      source(ConfigSource.empty, PropertyType.LocalDateTimeType) ?? "value of type localdatetime"

    /**
     * A config descriptor that describes retrieving a LocalDateTime from a given path.
     *
     * {{{
     *
     *     val mapSource =
     *      ConfigSource.fromMap(
     *         "time" : "2020-11-25T10:26:32.482299"
     *      )
     *
     *     val config = localDateTime("time")
     *     val result = read(config from mapSource)
     *
     *     // Right(2020-11-25T10:26:32.482299)
     *
     * }}}
     *
     */
    def localDateTime(path: String): ConfigDescriptor[LocalDateTime] = nested(path)(localDateTime)

    val localTime: ConfigDescriptor[LocalTime] =
      source(ConfigSource.empty, PropertyType.LocalTimeType) ?? "value of type localtime"

    /**
     * A config descriptor that describes retrieving a LocalTime from a given path.
     *
     * {{{
     *
     *     val mapSource =
     *      ConfigSource.fromMap(
     *         "LOCAL_TIME" : "10:29:02.278213"
     *      )
     *
     *     val config = localTime("LOCAL_TIME")
     *     val result = read(config from mapSource)
     *
     *     // Right(10:29:02.278213)
     *
     * }}}
     *
     */
    def localTime(path: String): ConfigDescriptor[LocalTime] = nested(path)(localTime)

    val long: ConfigDescriptor[Long] =
      source(ConfigSource.empty, PropertyType.LongType) ?? "value of type long"

    /**
     * A config descriptor that describes retrieving a Long from a given path.
     *
     * {{{
     *
     *     val mapSource =
     *      ConfigSource.fromMap(
     *         "COST" : 111111111
     *      )
     *
     *     val config = long("COST")
     *     val result = read(config from mapSource)
     *
     *     // Right(111111111)
     *
     * }}}
     *
     */
    def long(path: String): ConfigDescriptor[Long] = nested(path)(long)

    val short: ConfigDescriptor[Short] =
      source(ConfigSource.empty, PropertyType.ShortType) ?? "value of type short"

    /**
     * A config descriptor that describes retrieving a Short from a given path.
     *
     * {{{
     *
     *     val mapSource =
     *      ConfigSource.fromMap(
     *         "ID" : "1"
     *      )
     *
     *     val config = short("ID")
     *     val result = read(config from mapSource)
     *
     *     // Right(1)
     *
     * }}}
     *
     */
    def short(path: String): ConfigDescriptor[Short] = nested(path)(short)

    val string: ConfigDescriptor[String] =
      source(ConfigSource.empty, PropertyType.StringType) ?? "value of type string"

    /**
     * A config descriptor that describes retrieving a String from a given path.
     *
     * {{{
     *
     *     val mapSource =
     *      ConfigSource.fromMap(
     *         "COUNTRY" : "Australia"
     *      )
     *
     *     val config = string("COUNTRY")
     *     val result = read(config from mapSource)
     *
     *     // Right(Australia)
     *
     * }}}
     *
     */
    def string(path: String): ConfigDescriptor[String] = nested(path)(string)

    val uri: ConfigDescriptor[URI] =
      source(ConfigSource.empty, PropertyType.UriType) ?? "value of type uri"

    /**
     * A config descriptor that describes retrieving a Uri from a given path.
     *
     * {{{
     *
     *     val mapSource =
     *      ConfigSource.fromMap(
     *         "URI" : "www.bla.com"
     *      )
     *
     *     val config = uri("URI")
     *     val result = read(config from mapSource)
     *
     *     // Right(www.bla.com)
     *
     * }}}
     *
     */
    def uri(path: String): ConfigDescriptor[URI] = nested(path)(uri)

    val uuid: ConfigDescriptor[UUID] =
      source(ConfigSource.empty, PropertyType.UuidType) ?? "value of type uuid"

    /**
     * A config descriptor that describes retrieving a Uuid from a given path.
     *
     * {{{
     *
     *     val mapSource =
     *      ConfigSource.fromMap(
     *         "ID" : "a0f25f26-95b3-4124-8f7f-67fb04f714b7"
     *      )
     *
     *     val config = uuid("ID")
     *     val result = read(config from mapSource)
     *
     *     // Right(a0f25f26-95b3-4124-8f7f-67fb04f714b7)
     *
     * }}}
     *
     */
    def uuid(path: String): ConfigDescriptor[UUID] = nested(path)(uuid)

    val url: ConfigDescriptor[URL] =
      source(ConfigSource.empty, PropertyType.UrlType) ?? "value of type URL"

    /**
     * A config descriptor that describes retrieving a Url from a given path.
     *
     * {{{
     *
     *     val mapSource =
     *      ConfigSource.fromMap(
     *         "URL" : "www.bla.com"
     *      )
     *
     *     val config = bigInt("COST")
     *     val result = read(config from mapSource)
     *
     *     // Right(111111111)
     *
     * }}}
     *
     */
    def url(path: String): ConfigDescriptor[URL] = nested(path)(url)

    val zioDuration: ConfigDescriptor[zio.duration.Duration] =
      source(ConfigSource.empty, PropertyType.ZioDurationType) ?? "value of type duration"

    /**
     * A config descriptor that describes retrieving a zioDuration from a given path.
     *
     * {{{
     *
     *     val mapSource =
     *      ConfigSource.fromMap(
     *         "DURATION" : "3 seconds"
     *      )
     *
     *     val config = zioDuration("DURATION")
     *     val result = read(config from mapSource)
     *
     *     // Right(PT3S)
     *
     * }}}
     *
     */
    def zioDuration(path: String): ConfigDescriptor[zio.duration.Duration] = nested(path)(zioDuration)

    val javaFilePath: ConfigDescriptor[java.nio.file.Path] =
      source(ConfigSource.empty, PropertyType.JavaFilePathType) ?? "value of type java.nio.file.Path"

    /**
     * A config descriptor that describes retrieving a javaFilePath from a given path.
     *
     * {{{
     *
     *     val mapSource =
     *      ConfigSource.fromMap(
     *         "FILE_PATH" : "/Users/abc/xyz.txt"
     *      )
     *
     *     val config = javaFilePath("COST")
     *     val result = read(config from mapSource)
     *
     *     // Right(/Users/abc/xyz.txt)
     *
     * }}}
     *
     */
    def javaFilePath(path: String): ConfigDescriptor[java.nio.file.Path] =
      lazyDesc(nested(path)(javaFilePath))
  }

  /**
   * Use functions in this `Config` object when you need to retrieve your instance of config in terms of zio.Layer.
   *
   * For example:
   *
   * {{{
   *   final case class MyConfig(dburl: String, port: Int)
   *
   *   val myConfigDesc: ConfigDescriptor[MyConfig]             = (string("dburl") |@| int("port"))(MyConfig.apply, MyConfig.unapply)
   *   val myConfig: Layer[ReadError[String], Config[MyConfig]] = Config.fromSystemEnv(myConfigDesc)
   * }}}
   *
   * By using `Config.fromSystemEnv(myConfigDesc)`, it internally extends your description which is `myConfigDesc` to include the `ConfigSource`.
   * In the above example, it is the `ConfigSource`` corresponding to `sys.env`.
   * It then calls `zio.config.read` with this new description that includes the source information.
   *
   * Extending an existing config description to include a `ConfigSource` is as simple as
   *
   * {{{
   *   myConfigDesc from configSource
   * }}}
   *
   * Also, note that `Config[MyConfig]` in the above example is a simple type alias to `Has[MyConfig]`.
   *
   * If you want to retrieve your config as scala.Either instead of zio.Layer, then you will have to extend
   * your description to include the information on `ConfigSource` manually.
   *
   * For example:
   *
   * {{{
   *   import zio.config._, ConfigDescriptor._
   *   final case class MyConfig(dburl: String, port: Int)
   *
   *   val myConfig: ConfigDescriptor[MyConfig]         = (string("dburl") |@| int("port"))(MyConfig.apply, MyConfig.unapply)
   *   val constantSource: ConfigSource                 = ConfigSource.fromMap(Map("dburl" -> "xyz", "port" -> "8080"))
   *   val result: Either[ReadError[String], MyConfig]  = read(myConfig from constantSource)
   * }}}
   *
   * Note: With the above approach, we got a simple scala.Either instead of retrieving them in terms of ZIO.
   * Instead of the above approach, if we use `Config.fromMap(constantMap, myConfig)`,
   * then we will get a `Layer[ReadError[String], MyConfig]`
   *
   * The above approach is especially useful when we have a custom `ConfigSource`.
   * For instance, we can form a custom `ConfigSource` by composing a few existing ConfigSources.
   *
   * For example:
   *
   * {{{
   *   import zio.config._, ConfigDescriptor._
   *
   *   final case class MyConfig(dburl: String, port: Int)
   *
   *   val myConfig: ConfigDescriptor[MyConfig]     = (string("dburl") |@| int("port"))(MyConfig.apply, MyConfig.unapply)
   *   val sysEnvSource: UIO[MyConfig]              = ConfigSource.fromSystemEnv
   *   val constantSource: ConfigSource             = ConfigSource.fromMap(Map("dburl" -> "xyz", "port" -> "8080"))
   *   val result: IO[ReadError[String], MyConfig]  = configSource.flatMap(source => read(myConfig from sysEnvSource.orElse(constantSource))
   * }}}
   *
   * In the above example, the results returned an UIO because of the existence of ConfigSource` corresponding to `sys.env`.
   *
   */
  object ZConfig {

    /**
     * EXPERIMENTAL
     *
     * Forming configuration from command line arguments.
     *
     * Assumption. All keys should start with either -
     *
     * This source supports almost all standard command-line patterns including nesting/sub-config, repetition/list etc
     *
     * Example:
     *
     * Given:
     *
     * {{{
     *    args = "-db.username=1 --db.password=hi --vault -username=3 --vault -password=10 --regions 111,122 --user k1 --user k2"
     *    keyDelimiter   = Some('.')
     *    valueDelimiter = Some(',')
     * }}}
     *
     * then, the following works:
     *
     * {{{
     *
     *  final case class Credentials(username: String, password: String)
     *
     *  val credentials = (string("username") |@| string("password"))(Credentials.apply, Credentials.unapply)
     *
     *  final case class Config(databaseCredentials: Credentials, vaultCredentials: Credentials, regions: List[String], users: List[String])
     *
     *  (nested("db") { credentials } |@| nested("vault") { credentials } |@| list("regions")(string) |@| list("user")(string))(Config.apply, Config.unapply)
     *
     *  // res0 Config(Credentials(1, hi), Credentials(3, 10), List(111, 122), List(k1, k2))
     *
     * }}}
     *
     * @see [[https://github.com/zio/zio-config/tree/master/examples/src/main/scala/zio/config/examples/commandline/CommandLineArgsExample.scala]]
     */
    def fromCommandLineArgs[A](
      args: List[String],
      configDescriptor: ConfigDescriptor[A],
      keyDelimiter: Option[Char] = None,
      valueDelimiter: Option[Char] = None
    )(implicit tag: Tag[A]): Layer[ReadError[String], Has[A]] =
      fromConfigDescriptor(
        configDescriptor from ConfigSource.fromCommandLineArgs(args, keyDelimiter, valueDelimiter)
      )

    /**
     * Provide keyDelimiter if you need to consider flattened config as a nested config.
     * Provide valueDelimiter if you need any value to be a list
     *
     * Example:
     *
     * Given:
     *
     * {{{
     *    map            = Map("KAFKA_SERVERS" -> "server1, server2", "KAFKA_SERDE"  -> "confluent")
     *    keyDelimiter   = Some('_')
     *    valueDelimiter = Some(',')
     * }}}
     *
     * then, the following works:
     *
     * {{{
     *    final case class kafkaConfig(server: String, serde: String)
     *    nested("KAFKA")(string("SERVERS") |@| string("SERDE"))(KafkaConfig.apply, KafkaConfig.unapply)
     * }}}
     */
    def fromMap[A](
      map: Map[String, String],
      configDescriptor: ConfigDescriptor[A],
      source: String = "constant",
      keyDelimiter: Option[Char] = None,
      valueDelimiter: Option[Char] = None
    )(implicit tag: Tag[A]): Layer[ReadError[String], Has[A]] =
      fromConfigDescriptor(configDescriptor from ConfigSource.fromMap(map, source, keyDelimiter, valueDelimiter))

    /**
     * Provide keyDelimiter if you need to consider flattened config as a nested config.
     *
     * Example:
     *
     * Given:
     *
     * {{{
     *    map = Map("KAFKA_SERVERS" -> singleton(server1), "KAFKA_SERDE"  -> singleton("confluent"))
     *    keyDelimiter = Some('_')
     * }}}
     *
     * then, the following works:
     *
     * {{{
     *    final case class kafkaConfig(server: String, serde: String)
     *    nested("KAFKA")(string("SERVERS") |@| string("SERDE"))(KafkaConfig.apply, KafkaConfig.unapply)
     * }}}
     */
    def fromMultiMap[A](
      map: Map[String, ::[String]],
      configDescriptor: ConfigDescriptor[A],
      source: String,
      keyDelimiter: Option[Char] = None
    )(implicit tag: Tag[A]): Layer[ReadError[String], Has[A]] =
      fromConfigDescriptor(configDescriptor from ConfigSource.fromMultiMap(map, source, keyDelimiter))

    /**
     * Provide keyDelimiter if you need to consider flattened config as a nested config.
     * Provide valueDelimiter if you need any value to be a list
     *
     * Example:
     *
     * Given:
     *
     * {{{
     *    property      = "KAFKA.SERVERS" = "server1, server2" ; "KAFKA.SERDE" = "confluent"
     *    keyDelimiter   = Some('.')
     *    valueDelimiter = Some(',')
     * }}}
     *
     * then, the following works:
     *
     * {{{
     *    final case class kafkaConfig(server: String, serde: String)
     *    nested("KAFKA")(string("SERVERS") |@| string("SERDE"))(KafkaConfig.apply, KafkaConfig.unapply)
     * }}}
     */
    def fromProperties[A](
      properties: Properties,
      configDescriptor: ConfigDescriptor[A],
      source: String,
      keyDelimiter: Option[Char] = None,
      valueDelimiter: Option[Char] = None
    )(implicit tag: Tag[A]): Layer[ReadError[String], Has[A]] =
      fromConfigDescriptor(
        configDescriptor from ConfigSource.fromProperties(properties, source, keyDelimiter, valueDelimiter)
      )

    /**
     * Provide keyDelimiter if you need to consider flattened config as a nested config.
     * Provide valueDelimiter if you need any value to be a list
     *
     * Example:
     *
     * Given:
     *
     * {{{
     *    properties (in file) = "KAFKA.SERVERS" = "server1, server2" ; "KAFKA.SERDE" = "confluent"
     *    keyDelimiter         = Some('.')
     *    valueDelimiter       = Some(',')
     * }}}
     *
     * then, the following works:
     *
     * {{{
     *    final case class kafkaConfig(server: String, serde: String)
     *    nested("KAFKA")(string("SERVERS") |@| string("SERDE"))(KafkaConfig.apply, KafkaConfig.unapply)
     * }}}
     *
     */
    def fromPropertiesFile[A](
      filePath: String,
      configDescriptor: ConfigDescriptor[A],
      keyDelimiter: Option[Char] = None,
      valueDelimiter: Option[Char] = None
    )(implicit tag: Tag[A]): Layer[Throwable, Has[A]] =
      fromConfigDescriptorM(
        ConfigSource
          .fromPropertiesFile(filePath, keyDelimiter, valueDelimiter)
          .map(configDescriptor from _)
      )

    /**
     * Consider providing keyDelimiter if you need to consider flattened config as a nested config.
     * Consider providing valueDelimiter if you need any value to be a list
     *
     * Example:
     *
     * Given:
     *
     * {{{
     *    vars in sys.env  = "KAFKA_SERVERS" = "server1, server2" ; "KAFKA_SERDE" = "confluent"
     *    keyDelimiter     = Some('_')
     *    valueDelimiter   = Some(',')
     * }}}
     *
     * then, the following works:
     *
     * {{{
     *    final case class kafkaConfig(server: String, serde: String)
     *    nested("KAFKA")(string("SERVERS") |@| string("SERDE"))(KafkaConfig.apply, KafkaConfig.unapply)
     * }}}
     *
     * Note: The delimiter '.' for keys doesn't work in system environment.
     */
    def fromSystemEnv[K, V, A](
      configDescriptor: ConfigDescriptor[A],
      keyDelimiter: Option[Char] = None,
      valueDelimiter: Option[Char] = None
    )(implicit tag: Tag[A]): ZLayer[System, ReadError[String], Has[A]] =
      fromConfigDescriptorM(ConfigSource.fromSystemEnv(keyDelimiter, valueDelimiter).map(configDescriptor from _))

    /**
     * Consider providing keyDelimiter if you need to consider flattened config as a nested config.
     * Consider providing valueDelimiter if you need any value to be a list
     *
     * Example:
     *
     * Given:
     *
     * {{{
     *    vars in sys.env  = "KAFKA.SERVERS" = "server1, server2" ; "KAFKA.SERDE" = "confluent"
     *    keyDelimiter     = Some('.')
     *    valueDelimiter   = Some(',')
     * }}}
     *
     * then, the following works:
     *
     * {{{
     *    final case class kafkaConfig(server: String, serde: String)
     *    nested("KAFKA")(string("SERVERS") |@| string("SERDE"))(KafkaConfig.apply, KafkaConfig.unapply)
     * }}}
     *
     */
    def fromSystemProperties[K, V, A](
      configDescriptor: ConfigDescriptor[A],
      keyDelimiter: Option[Char] = None,
      valueDelimiter: Option[Char] = None
    )(implicit tag: Tag[A]): ZLayer[System, ReadError[String], Has[A]] =
      fromConfigDescriptorM(
        ConfigSource.fromSystemProperties(keyDelimiter, valueDelimiter).map(configDescriptor from _)
      )

    private[config] def fromConfigDescriptor[A](
      configDescriptor: ConfigDescriptor[A]
    )(implicit tag: Tag[A]): Layer[ReadError[K], Has[A]] =
      ZLayer.fromEffect(ZIO.fromEither(read(configDescriptor)))

    private[config] def fromConfigDescriptorM[R, E >: ReadError[K], A](
      configDescriptor: ZIO[R, E, ConfigDescriptor[A]]
    )(implicit tag: Tag[A]): ZLayer[R, E, Has[A]] =
      ZLayer.fromEffect(
        configDescriptor.flatMap(
          descriptor => ZIO.fromEither(read(descriptor))
        )
      )
  }

}
