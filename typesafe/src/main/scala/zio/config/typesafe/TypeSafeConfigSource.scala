package zio.config.typesafe

import com.typesafe.config.ConfigFactory
import zio.config.ConfigSource

import zio.{ ZIO }
import java.io.File
import zio.config._
import com.typesafe.config.ConfigValueType
import scala.util.Try
import scala.util.Failure
import scala.util.Success
import scala.collection.JavaConverters._

// TODO; Experimental: Yet to refactor
object TypeSafeConfigSource {
  // It is user's responsiblility to load the file
  def hoccon(
    hoccon: Either[File, String]
  ): ConfigSource[String, String] =
    ConfigSource(
      (path: Vector[String]) => {
        // At each entry level in the path, we see if it's a list or not.
        val config =
          hoccon.fold(file => ConfigFactory.parseFile(file).resolve, str => ConfigFactory.parseString(str).resolve)

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
                    new RuntimeException(s"The value for the key ${path} is an object and not a primitive.")
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

        def loop(
          parentConfig: com.typesafe.config.Config,
          list: List[String],
          nextPath: List[String]
        ): Either[ReadErrorsVector[String, String], ::[String]] =
          list match {
            case Nil =>
              nextPath.lastOption match {
                case Some(lastKey) => getValue(parentConfig, parentConfig.getValue(lastKey).valueType(), lastKey)
                case None          => Left(singleton(ReadError.missingValue[Vector[String], String](path)))
              }

            case head :: next => {
              if (parentConfig.getValue(head).valueType() == ConfigValueType.LIST) {
                val list = parentConfig.getConfigList(head).asScala.toList
                val r    = list.map(eachConfig => loop(eachConfig, next, nextPath :+ head))

                seqEither(r)
                  .map(t => t.flatMap(_.toList))
                  .flatMap({
                    case Nil =>
                      Left(singleton(ReadError.missingValue[Vector[String], String](path)))
                    case h :: t => Right(::(h, t))
                  })
              } else {
                if (parentConfig.getValue(head).valueType() == ConfigValueType.OBJECT) {
                  loop(parentConfig.getConfig(head), next, nextPath :+ head)
                } else if (next.isEmpty) {
                  loop(parentConfig, next, nextPath :+ head)
                } else {
                  Left(singleton(ReadError.missingValue[Vector[String], String](path)))
                }
              }
            }
          }

        ZIO.fromEither(loop(config, path.toList, Nil)).map(t => ConfigValue(t, "typesafe-config-hoccon"))
      },
      List("typesafe-config-hoccon")
    )

  private def asListOfString[A](jlist: => java.util.List[A]): Try[List[String]] =
    Try(jlist).map(_.asScala.toList.map(_.toString))
}
