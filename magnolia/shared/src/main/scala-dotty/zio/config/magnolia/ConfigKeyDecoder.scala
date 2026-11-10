package zio.config.magnolia

import zio.Config

import java.util.UUID
import scala.util.Try

trait ConfigKeyDecoder[A] {
  def decode(key: String): Either[Config.Error, A]
}

object ConfigKeyDecoder {
  def apply[A](using ev: ConfigKeyDecoder[A]): ConfigKeyDecoder[A] = ev

  given ConfigKeyDecoder[String] = (key: String) => Right(key)

  given ConfigKeyDecoder[UUID] = (key: String) =>
    Try(UUID.fromString(key)).toEither.left
      .map(_ => Config.Error.InvalidData(message = s"Expected a UUID but found: $key"))

  given ConfigKeyDecoder[Int] = (key: String) =>
    Try(key.toInt).toEither.left
      .map(_ => Config.Error.InvalidData(message = s"Expected an Int but found: $key"))

  given ConfigKeyDecoder[Long] = (key: String) =>
    Try(key.toLong).toEither.left
      .map(_ => Config.Error.InvalidData(message = s"Expected a Long but found: $key"))
}
