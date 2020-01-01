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

// Experimental: Yet to refactor
object TypeSafeConfigSource {
  // It is user's responsiblility to load the file
  def hoccon(
    hoccon: Either[File, String],
    configParseOptions: Option[ConfigParseOptions] = None
  ): ConfigSource[String, String] =
    ConfigSource(
      (path: Vector[String]) => {
        // At each entry level in the path, we see if it's a list or not.
        val config =
          hoccon.fold(file => ConfigFactory.parseFile(file).resolve, str => ConfigFactory.parseString(str).resolve)

        val parentKey =
          (if (path.size > 1) path.dropRight(1) else path).mkString(".")

        val originalKey =
          path.mkString(".")

        val parentValue = config.getValue(parentKey)

        val getValue: (
          com.typesafe.config.Config,
          ConfigValueType,
          String
        ) => Either[ReadErrorsVector[String, String], ::[String]] =
          (config, valueType, key) =>
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
                    new RuntimeException(s"The value for the key ${originalKey} is an object and not a primitive.")
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

        // This is simple testing
        val result =
          if (parentValue.valueType() == ConfigValueType.LIST) {
            val lastKey = path.last
            val list    = config.getConfigList(parentKey).asScala.toList
            seqEither(list.map(eachConfig => {
              getValue(eachConfig, eachConfig.getValue(lastKey).valueType(), lastKey)
            })).flatMap {
              case Nil =>
                Left(singleton(ReadError.MissingValue(path))): Either[ReadErrorsVector[String, String], ::[String]]
              case h :: t => Right(::(h.head, h.tail ++ t.flatten))
            }
          } else {
            getValue(config, config.getValue(originalKey).valueType(), originalKey)
          }

        ZIO.fromEither(result).map(t => ConfigValue(t, "typesafe-config-hoccon"))
      },
      List("typesafe-config-hoccon")
    )

  private def asListOfString[A](jlist: => java.util.List[A]): Try[List[String]] =
    Try(jlist).map(_.asScala.toList.map(_.toString))
}
