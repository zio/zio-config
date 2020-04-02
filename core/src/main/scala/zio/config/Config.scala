package zio.config

import zio.system.System
import zio.{Layer, Tagged, ZIO, ZLayer}

object Config {

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

  //  def fromArgs[A](
  //    configDescriptor: ConfigDescriptor[String, String, A],
  //    args: List[String],
  //    valueDelimiter: Option[String] = None
  //  )(implicit tagged: Tagged[Service[A]]): ZLayer[Any, ReadErrors[Vector[String], String], Config[A]] =
  //    fromConfigDescriptor(ConfigSource.fromArgs(args, valueDelimiter), configDescriptor)

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
