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
  )(implicit tagged: Tagged[A]): Layer[ReadError, Config[A]] =
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
  )(implicit tagged: Tagged[A]): Layer[ReadError, Config[A]] =
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
  )(implicit tagged: Tagged[A]): Layer[ReadError, Config[A]] =
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
  )(implicit tagged: Tagged[A]): Layer[ReadError, Config[A]] =
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
  def fromSystemEnv[A](
    configDescriptor: ConfigDescriptor[A],
    keyDelimiter: Option[Char] = None,
    valueDelimiter: Option[Char] = None
  )(implicit tagged: Tagged[A]): Layer[ReadError, Config[A]] =
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
  def fromSystemProperties[A](
    configDescriptor: ConfigDescriptor[A],
    keyDelimiter: Option[Char] = None,
    valueDelimiter: Option[Char] = None
  )(implicit tagged: Tagged[A]): ZLayer[System, ReadError, Config[A]] =
    fromConfigDescriptorM(ConfigSource.fromSystemProperties(keyDelimiter, valueDelimiter).map(configDescriptor.from))

  private[config] def fromConfigDescriptor[A](
    configDescriptor: ConfigDescriptor[A]
  )(implicit tagged: Tagged[A]): Layer[ReadError, Config[A]] =
    ZLayer.fromEffect(ZIO.fromEither(read(configDescriptor)))

  private[config] def fromConfigDescriptorM[R, E >: ReadError, A](
    configDescriptor: ZIO[R, E, ConfigDescriptor[A]]
  )(implicit tagged: Tagged[A]): ZLayer[R, E, Config[A]] =
    ZLayer.fromEffect(
      configDescriptor.flatMap(
        descriptor => ZIO.fromEither(read(descriptor))
      )
    )

}
