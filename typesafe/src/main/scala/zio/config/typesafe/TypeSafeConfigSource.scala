package zio.config.typesafe

import com.typesafe.config.ConfigFactory
import zio.config.ConfigSource

import zio.{ ZIO }
import java.io.File
import zio.config._
import com.typesafe.config.ConfigParseOptions
import com.typesafe.config.ConfigValueType
import scala.util.Try
import scala.util.Failure
import scala.util.Success
import scala.jdk.CollectionConverters._

object TypeSafeConfigSource {
  // It is user's responsiblility to load the file
  def fromHoccon(
    hoccon: Either[File, String],
    configParseOptions: Option[ConfigParseOptions] = None
  ): ConfigSource[String, String] =
    ConfigSource(
      (path: Vector[String]) => {
        // At each entry level in the path, we see if it's a list or not.

        val config =
          hoccon.fold(file => ConfigFactory.parseFile(file), str => ConfigFactory.parseString(str))

        val key =
          path.mkString(".")

        val value = config.getValue(key)

        val result: ConfigValueType => Either[ReadErrorsVector[String, String], ::[String]] =
          valueType =>
            if (valueType == ConfigValueType.BOOLEAN) {
              Right(singleton(config.getBoolean(key).toString()))
            } else if (valueType == ConfigValueType.NULL) {
              Left(singleton(ReadError.MissingValue(path)))
            } else if (valueType == ConfigValueType.NUMBER) {
              Right(singleton(config.getNumber(key).toString()))
            } else if (valueType == ConfigValueType.STRING) {
              Right(singleton(config.getString(key)))
            } else if (valueType == ConfigValueType.OBJECT) {
              Left(
                singleton(
                  ReadError.FatalError(
                    path,
                    new RuntimeException(s"The value for the key ${key} is an object and not a primitive.")
                  )
                )
              )
            } else if (valueType == ConfigValueType.LIST) {
              asListOfString(config.getStringList(key))
                .orElse(
                  asListOfString(config.getIntList(key))
                )
                .orElse(
                  asListOfString(config.getBooleanList(key))
                )
                .orElse(
                  asListOfString(config.getDurationList(key))
                )
                .orElse(
                  asListOfString(config.getBytesList(key))
                )
                .orElse(
                  asListOfString(config.getDoubleList(key))
                )
                .orElse(
                  asListOfString(config.getLongList(key))
                )
                .orElse(
                  asListOfString(config.getMemorySizeList(key))
                ) match {
                case Failure(exception) => Left(singleton(ReadError.FatalError(path, exception)))
                case Success(value) =>
                  value match {
                    case h :: t => Right(::(h, t))
                    case Nil    => Left(singleton(ReadError.MissingValue(path)))
                  }
              }
            } else {
              Left(singleton(ReadError.FatalError(path, new RuntimeException("Unknown type"))))
            }

        ZIO.fromEither(result(value.valueType()).map(t => ConfigValue(t, "typesafe-config-hoccon")))
      },
      List("typesafe-config-hoccon")
    )

  private def asListOfString[A](jlist: => java.util.List[A]): Try[List[String]] =
    Try(jlist).map(_.asScala.toList.map(_.toString))
}
