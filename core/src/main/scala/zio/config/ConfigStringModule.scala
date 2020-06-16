package zio.config

import java.io.File
import java.net.{ URI, URL }
import java.time.{ Instant, LocalDate, LocalDateTime, LocalTime }
import java.util.UUID
import java.util.Properties
import zio.{ Layer, Tag }
import zio.system.System
import zio.{ ZIO, ZLayer }

import scala.concurrent.duration.Duration

trait ConfigStringModule extends ConfigModule with ConfigSourceStringModule {

  object ConfigDescriptor extends ConfigDescriptorFunctions {
    import ConfigDescriptorAdt._

    val bigDecimal: ConfigDescriptor[BigDecimal] =
      Source(ConfigSource.empty, PropertyType.BigDecimalType) ?? "value of type bigdecimal"

    def bigDecimal(path: String): ConfigDescriptor[BigDecimal] = nested(path)(bigDecimal)

    val bigInt: ConfigDescriptor[BigInt] =
      Source(ConfigSource.empty, PropertyType.BigIntType) ?? "value of type bigint"

    def bigInt(path: String): ConfigDescriptor[BigInt] = nested(path)(bigInt)

    val boolean: ConfigDescriptor[Boolean] =
      Source(ConfigSource.empty, PropertyType.BooleanType) ?? "value of type boolean"

    def boolean(path: String): ConfigDescriptor[Boolean] = nested(path)(boolean)

    val byte: ConfigDescriptor[Byte] =
      Source(ConfigSource.empty, PropertyType.ByteType) ?? "value of type byte"

    def byte(path: String): ConfigDescriptor[Byte] = nested(path)(byte)

    val double: ConfigDescriptor[Double] =
      Source(ConfigSource.empty, PropertyType.DoubleType) ?? "value of type double"

    def double(path: String): ConfigDescriptor[Double] = nested(path)(double)

    val duration: ConfigDescriptor[Duration] =
      Source(ConfigSource.empty, PropertyType.DurationType) ?? "value of type duration"

    def duration(path: String): ConfigDescriptor[Duration] = nested(path)(duration)

    val file: ConfigDescriptor[File] =
      Source(ConfigSource.empty, PropertyType.FileType) ?? "value of type file"

    def file(path: String): ConfigDescriptor[File] = nested(path)(file)

    val float: ConfigDescriptor[Float] =
      Source(ConfigSource.empty, PropertyType.FloatType) ?? "value of type float"

    def float(path: String): ConfigDescriptor[Float] = nested(path)(float)

    val instant: ConfigDescriptor[Instant] =
      Source(ConfigSource.empty, PropertyType.InstantType) ?? "value of type instant"

    def instant(path: String): ConfigDescriptor[Instant] = nested(path)(instant)

    val int: ConfigDescriptor[Int] =
      Source(ConfigSource.empty, PropertyType.IntType) ?? "value of type int"

    def int(path: String): ConfigDescriptor[Int] = nested(path)(int)

    val localDate: ConfigDescriptor[LocalDate] =
      Source(ConfigSource.empty, PropertyType.LocalDateType) ?? "value of type localdate"

    def localDate(path: String): ConfigDescriptor[LocalDate] = nested(path)(localDate)

    val localDateTime: ConfigDescriptor[LocalDateTime] =
      Source(ConfigSource.empty, PropertyType.LocalDateTimeType) ?? "value of type localdatetime"

    def localDateTime(path: String): ConfigDescriptor[LocalDateTime] = nested(path)(localDateTime)

    val localTime: ConfigDescriptor[LocalTime] =
      Source(ConfigSource.empty, PropertyType.LocalTimeType) ?? "value of type localtime"

    def localTime(path: String): ConfigDescriptor[LocalTime] = nested(path)(localTime)

    val long: ConfigDescriptor[Long] =
      Source(ConfigSource.empty, PropertyType.LongType) ?? "value of type long"

    def long(path: String): ConfigDescriptor[Long] = nested(path)(long)

    val short: ConfigDescriptor[Short] =
      Source(ConfigSource.empty, PropertyType.ShortType) ?? "value of type short"

    def short(path: String): ConfigDescriptor[Short] = nested(path)(short)

    val string: ConfigDescriptor[String] =
      Source(ConfigSource.empty, PropertyType.StringType) ?? "value of type string"

    def string(path: String): ConfigDescriptor[String] = nested(path)(string)

    val uri: ConfigDescriptor[URI] =
      Source(ConfigSource.empty, PropertyType.UriType) ?? "value of type uri"

    def uri(path: String): ConfigDescriptor[URI] = nested(path)(uri)

    val uuid: ConfigDescriptor[UUID] =
      Source(ConfigSource.empty, PropertyType.UuidType) ?? "value of type uuid"

    def uuid(path: String): ConfigDescriptor[UUID] = nested(path)(uuid)

    val url: ConfigDescriptor[URL] =
      Source(ConfigSource.empty, PropertyType.UrlType) ?? "value of type URL"

    def url(path: String): ConfigDescriptor[URL] = nested(path)(url)

    val zioDuration: ConfigDescriptor[zio.duration.Duration] =
      Source(ConfigSource.empty, PropertyType.ZioDurationType) ?? "value of type duration"

    def zioDuration(path: String): ConfigDescriptor[zio.duration.Duration] = nested(path)(zioDuration)

    val javaFilePath: ConfigDescriptor[java.nio.file.Path] =
      Source(ConfigSource.empty, PropertyType.JavaFilePathType) ?? "value of type java.nio.file.Path"

    def javaFilePath(path: String): ConfigDescriptor[java.nio.file.Path] = nested(path)(javaFilePath)
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
  object Config {

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
    )(implicit tag: Tag[A]): Layer[ReadError[String], Config[A]] =
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
    )(implicit tag: Tag[A]): Layer[ReadError[String], Config[A]] =
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
    )(implicit tag: Tag[A]): Layer[ReadError[String], Config[A]] =
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
    )(implicit tag: Tag[A]): Layer[ReadError[String], Config[A]] =
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
    )(implicit tag: Tag[A]): Layer[Throwable, Config[A]] =
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
    )(implicit tag: Tag[A]): Layer[ReadError[String], Config[A]] =
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
    )(implicit tag: Tag[A]): ZLayer[System, ReadError[String], Config[A]] =
      fromConfigDescriptorM(
        ConfigSource.fromSystemProperties(keyDelimiter, valueDelimiter).map(configDescriptor from _)
      )

    private[config] def fromConfigDescriptor[A](
      configDescriptor: ConfigDescriptor[A]
    )(implicit tag: Tag[A]): Layer[ReadError[K], Config[A]] =
      ZLayer.fromEffect(ZIO.fromEither(read(configDescriptor)))

    private[config] def fromConfigDescriptorM[R, E >: ReadError[K], A](
      configDescriptor: ZIO[R, E, ConfigDescriptor[A]]
    )(implicit tag: Tag[A]): ZLayer[R, E, Config[A]] =
      ZLayer.fromEffect(
        configDescriptor.flatMap(
          descriptor => ZIO.fromEither(read(descriptor))
        )
      )
  }

}
