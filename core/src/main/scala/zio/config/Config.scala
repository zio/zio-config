package zio.config

import java.util.Properties

import zio.system.System
import zio.{ Layer, Tagged, ZIO, ZLayer }

object Config {

  /**
   * EXPERIMENTAL
   *
   * Forming configuration from command line arguments.
   * Assumption. All keys should start with either `-` or `--`
   *
   * Apart from the above assumptions, the source works similar to other sources, while adhering to well-known command-line arguments patterns.
   *
   * Example:
   *
   * Given:
   * {{{
   *  args = "-database.username=1 --database.password=hi --database.url=jdbc://xyz --vault -username=3 --vault -password=10 --users 100 --regions 111,122"
   *  keyDelimiter   = Some('.')
   *  valueDelimiter = Some(',')
   * }}}
   *
   * then, the below config will work
   *
   *  val credentials = (string("username") |@| string("password"))(Credentials.apply, Credentials.unapply)
   *  nested("database") { credentials } |@| nested("vault") { credentials } |@| list(string("regions") (Config.apply, Config.unapply)
   *
   * There is more that is in progress with this implementation.
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
   * Given
   * {{{
   *   map            = Map("KAFKA_SERVERS" -> "server1, server2", "KAFKA_SERIALIZERS"  -> "confluent")
   *   keyDelimiter   = Some('_')
   *   valueDelimiter = Some(',')
   * }}}
   *
   * then, the below config will work
   *  nested("KAFKA")(string("SERVER") |@| string("FLAG"))(KafkaConfig.apply, KafkaConfig.unapply)
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
   * Given
   * {{{
   *   map = Map("KAFKA_SERVERS" -> singleton(server1), "KAFKA_SERIALIZERS"  -> singleton("confluent"))
   *   keyDelimiter = Some('_')
   * }}}
   *
   * then, the below config will work
   *  nested("KAFKA")(string("SERVER") |@| string("FLAG"))(KafkaConfig.apply, KafkaConfig.unapply)
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
   * Given
   * {{{
   *   property      = "KAFKA.SERVERS" = "server1, server2" ; "KAFKA.SERIALIZERS" = "confluent"
   *   keyDelimiter   = Some('.')
   *   valueDelimiter = Some(',')
   * }}}
   *
   * then, the below config will work
   *  nested("KAFKA")(string("SERVER") |@| string("FLAG"))(KafkaConfig.apply, KafkaConfig.unapply)
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
   * Given
   * {{{
   *   properties (in file) = "KAFKA.SERVERS" = "server1, server2" ; "KAFKA.SERIALIZERS" = "confluent"
   *   keyDelimiter         = Some('.')
   *   valueDelimiter       = Some(',')
   * }}}
   *
   * then, the below config will work
   *  nested("KAFKA")(string("SERVER") |@| string("FLAG"))(KafkaConfig.apply, KafkaConfig.unapply)
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
   * Given
   * {{{
   *   vars in sys.env  = "KAFKA_SERVERS" = "server1, server2" ; "KAFKA_SERIALIZERS" = "confluent"
   *   keyDelimiter     = Some('_')
   *   valueDelimiter   = Some(',')
   * }}}
   *
   * then, the below config will work
   *  nested("KAFKA")(string("SERVER") |@| string("FLAG"))(KafkaConfig.apply, KafkaConfig.unapply)
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
   * Given
   * {{{
   *   vars in sys.env  = "KAFKA.SERVERS" = "server1, server2" ; "KAFKA.SERIALIZERS" = "confluent"
   *   keyDelimiter     = Some('.')
   *   valueDelimiter   = Some(',')
   * }}}
   *
   * then, the below config will work
   *  nested("KAFKA")(string("SERVER") |@| string("FLAG"))(KafkaConfig.apply, KafkaConfig.unapply)
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
