package zio.config

sealed trait ReadError[+K, +V]

object ReadError {
  final case class MissingValue[K](key: K, positions: Option[Int] = None)                    extends ReadError[K, Nothing]
  final case class ParseError[K, V](key: K, provided: V, message: String)                    extends ReadError[K, V]
  final case class Unknown[K](key: K, cause: Throwable)                                      extends ReadError[K, Nothing]
  final case class OrErrors[K, V](leftErrors: ReadError[K, V], rightErrors: ReadError[K, V]) extends ReadError[K, V]
  final case class AndErrors[K, V](errors: ::[ReadError[K, V]])                              extends ReadError[K, V]

  /**
   * If the value for a key is missing in ConfigSource, we use MissingValue
   *
   * @param key: The path for which the value is missing
   * @param position For some key, there will be values supposed to be existing in multiple positions in the config source. The starting pos is 0.
   * @tparam K
   */
  final def missingValue[K, V](key: K, position: Option[Int]): ReadError[K, V] =
    MissingValue(key, position)

  /**
   * If parsing a value to a type result in any error, we use ParseError
   *
   * @param key: The path for which the value exists, but has invalid values and cannot be parsed to a type.
   * @param provided: The actual value provided
   * @param message: Any extra custom messages.
   */
  final def parseError[K, V](key: K, provided: V, message: String): ReadError[K, V] =
    ParseError(key, provided, message)

  /**
   * If fetching a value for a key result in any non-fatal errors that are unknown, then we use Unknown error channel.
   *
   * @param key : The key for which the unknown error occurred.
   * @param cause: Cause of error that can only be a Throwable
   * @tparam K
   */
  final def unknownError[K, V](key: K, cause: Throwable): ReadError[K, V] =
    Unknown(key, cause)
}
