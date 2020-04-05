package zio.config

import java.util.Properties

import zio.system.System
import zio.{ Layer, Tagged, ZIO, ZLayer }

object Config {

  /**
   * EXPERIMENTAL
   *
   * Forming configuration from command line arguments.
   *
   * Assumption. All keys should start with either - or --
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
   *  final case class Config(databaseCredentials: Credentials, vaultCredentials: Credentials, regions: List[String, users: List[String])
   *
   *  (nested("db") { credentials } |@| nested("vault") { credentials } |@| list(string("regions") |@| list(string("user"))(Config.apply, Config.unapply)
   *
   *  // res0 Config(Credentials(1, hi), Credentials(3, 10), List(111, 122), List(k1, k2))
   *
   * }}}
   *
   * @see [[https://github.com/zio/zio-config/tree/master/examples/src/main/scala/zio/config/examples/commandline/CommandLineArgsExample.scala]]
   */
  def fromCommandLineArgs[K, V, A](
    args: List[String],
    configDescriptor: ConfigDescriptor[String, String, A],
    keyDelimiter: Option[Char] = None,
    valueDelimiter: Option[Char] = None
  )(implicit tagged: Tagged[A]): Layer[ReadError[String], Config[A]] =
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
   *    map            = Map("KAFKA_SERVERS" -> "server1, server2", "KAFKA_SERIALIZERS"  -> "confluent")
   *    keyDelimiter   = Some('_')
   *    valueDelimiter = Some(',')
   * }}}
   *
   * then, the following works:
   *
   * {{{
   *    nested("KAFKA")(string("SERVER") |@| string("FLAG"))(KafkaConfig.apply, KafkaConfig.unapply)
   * }}}
   */
  def fromMap[A](
    map: Map[String, String],
    configDescriptor: ConfigDescriptor[String, String, A],
    source: String = "constant",
    keyDelimiter: Option[Char] = None,
    valueDelimiter: Option[Char] = None
  )(implicit tagged: Tagged[A]): Layer[ReadError[String], Config[A]] =
    fromConfigDescriptor(configDescriptor from ConfigSource.fromMap(map, source, keyDelimiter, valueDelimiter))

  /**
   * Provide keyDelimiter if you need to consider flattened config as a nested config.
   *
   * Example:
   *
   * Given:
   *
   * {{{
   *    map = Map("KAFKA_SERVERS" -> singleton(server1), "KAFKA_SERIALIZERS"  -> singleton("confluent"))
   *    keyDelimiter = Some('_')
   * }}}
   *
   * then, the following works:
   *
   * {{{
   *    nested("KAFKA")(string("SERVER") |@| string("FLAG"))(KafkaConfig.apply, KafkaConfig.unapply)
   * }}}
   */
  def fromMultiMap[A](
    map: Map[String, ::[String]],
    configDescriptor: ConfigDescriptor[String, String, A],
    source: String,
    keyDelimiter: Option[Char] = None
  )(implicit tagged: Tagged[A]): Layer[ReadError[String], Config[A]] =
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
   *    property      = "KAFKA.SERVERS" = "server1, server2" ; "KAFKA.SERIALIZERS" = "confluent"
   *    keyDelimiter   = Some('.')
   *    valueDelimiter = Some(',')
   * }}}
   *
   * then, the following works:
   *
   * {{{
   *    nested("KAFKA")(string("SERVER") |@| string("FLAG"))(KafkaConfig.apply, KafkaConfig.unapply)
   * }}}
   */
  def fromProperties[A](
    properties: Properties,
    configDescriptor: ConfigDescriptor[String, String, A],
    source: String,
    keyDelimiter: Option[Char] = None,
    valueDelimiter: Option[Char] = None
  )(implicit tagged: Tagged[A]): Layer[ReadError[String], Config[A]] =
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
   *    properties (in file) = "KAFKA.SERVERS" = "server1, server2" ; "KAFKA.SERIALIZERS" = "confluent"
   *    keyDelimiter         = Some('.')
   *    valueDelimiter       = Some(',')
   * }}}
   *
   * then, the following works:
   *
   * {{{
   *    nested("KAFKA")(string("SERVER") |@| string("FLAG"))(KafkaConfig.apply, KafkaConfig.unapply)
   * }}}
   *
   */
  def fromPropertiesFile[A](
    filePath: String,
    configDescriptor: ConfigDescriptor[String, String, A],
    keyDelimiter: Option[Char] = None,
    valueDelimiter: Option[Char] = None
  )(implicit tagged: Tagged[A]): Layer[Throwable, Config[A]] =
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
   *    vars in sys.env  = "KAFKA_SERVERS" = "server1, server2" ; "KAFKA_SERIALIZERS" = "confluent"
   *    keyDelimiter     = Some('_')
   *    valueDelimiter   = Some(',')
   * }}}
   *
   * then, the following works:
   *
   * {{{
   *    nested("KAFKA")(string("SERVER") |@| string("FLAG"))(KafkaConfig.apply, KafkaConfig.unapply)
   * }}}
   *
   * Note: The delimiter '.' for keys doesn't work in system environment.
   */
  def fromSystemEnv[K, V, A](
    configDescriptor: ConfigDescriptor[String, String, A],
    keyDelimiter: Option[Char] = None,
    valueDelimiter: Option[Char] = None
  )(implicit tagged: Tagged[A]): Layer[ReadError[String], Config[A]] =
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
   *    vars in sys.env  = "KAFKA.SERVERS" = "server1, server2" ; "KAFKA.SERIALIZERS" = "confluent"
   *    keyDelimiter     = Some('.')
   *    valueDelimiter   = Some(',')
   * }}}
   *
   * then, the following works:
   *
   * {{{
   *    nested("KAFKA")(string("SERVER") |@| string("FLAG"))(KafkaConfig.apply, KafkaConfig.unapply)
   * }}}
   *
   */
  def fromSystemProperties[K, V, A](
    configDescriptor: ConfigDescriptor[String, String, A],
    keyDelimiter: Option[Char] = None,
    valueDelimiter: Option[Char] = None
  )(implicit tagged: Tagged[A]): ZLayer[System, ReadError[String], Config[A]] =
    fromConfigDescriptorM(ConfigSource.fromSystemProperties(keyDelimiter, valueDelimiter).map(configDescriptor from _))

  private[config] def fromConfigDescriptor[K, V, A](
    configDescriptor: ConfigDescriptor[K, V, A]
  )(implicit tagged: Tagged[A]): Layer[ReadError[K], Config[A]] =
    ZLayer.fromEffect(ZIO.fromEither(read(configDescriptor)))

  private[config] def fromConfigDescriptorM[R, E >: ReadError[K], K, V, A](
    configDescriptor: ZIO[R, E, ConfigDescriptor[K, V, A]]
  )(implicit tagged: Tagged[A]): ZLayer[R, E, Config[A]] =
    ZLayer.fromEffect(
      configDescriptor.flatMap(
        descriptor => ZIO.fromEither(read(descriptor))
      )
    )

}
