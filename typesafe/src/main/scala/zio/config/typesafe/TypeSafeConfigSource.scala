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

object TypeSafeConfigSource {
  def hoccon(
    hoccon: Either[File, String]
  ): ConfigSource[String, String] =
    ConfigSource(
      (path: Vector[String]) => {
        def effect[A](f: => A): Either[ReadErrorsVector[String, String], A] =
          Try(f) match {
            case Success(value)     => Right(value)
            case Failure(exception) => Left(singleton(ReadError.fatalError(path, exception)))
          }

        val getValue: (
          com.typesafe.config.Config,
          ConfigValueType,
          String
        ) => Either[ReadErrorsVector[String, String], ::[String]] =
          (config, valueType, key) =>
            if (valueType == ConfigValueType.BOOLEAN) {
              effect(config.getBoolean(key).toString()).map(singleton)
            } else if (valueType == ConfigValueType.NULL) {
              Left(singleton(ReadError.MissingValue(path)))
            } else if (valueType == ConfigValueType.NUMBER) {
              effect(config.getNumber(key).toString()).map(singleton)
            } else if (valueType == ConfigValueType.STRING) {
              effect(config.getString(key)).map(singleton)
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
                case Failure(exception) =>
                  Left(
                    singleton(
                      ReadError.FatalError(
                        path,
                        new RuntimeException(
                          "Trying to parse a list of config. However, the type is unidentified. Supports only [list] of [int, boolean, duration, bytes, double, long, memory size]",
                          exception
                        )
                      )
                    )
                  )
                case Success(value) =>
                  value match {
                    case h :: t => Right(::(h, t))
                    case Nil =>
                      Left(
                        singleton(
                          ReadError.MissingValue(path)
                        )
                      )
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
              for {
                valueType <- effect(parentConfig.getValue(head).valueType())
                res <- if (valueType == ConfigValueType.LIST) {
                        for {
                          listOfConfig <- effect(parentConfig.getConfigList(head).asScala.toList)
                          res <- seqEither(listOfConfig.map(eachConfig => loop(eachConfig, next, nextPath :+ head)))
                                  .map(t => t.flatMap(_.toList))
                                  .flatMap({
                                    case Nil =>
                                      Left(singleton(ReadError.missingValue[Vector[String], String](path)))
                                    case h :: t => Right(::(h, t))
                                  })

                        } yield res
                      } else {
                        if (parentConfig.getValue(head).valueType() == ConfigValueType.OBJECT) {
                          loop(parentConfig.getConfig(head), next, nextPath :+ head)
                        } else if (next.isEmpty) {
                          loop(parentConfig, next, nextPath :+ head)
                        } else {
                          Left(singleton(ReadError.missingValue[Vector[String], String](path)))
                        }
                      }
              } yield res
            }
          }

        for {
          config <- ZIO
                     .effect(
                       hoccon.fold(
                         file => ConfigFactory.parseFile(file).resolve,
                         str => ConfigFactory.parseString(str).resolve
                       )
                     )
                     .mapError(throwable => singleton(ReadError.fatalError[Vector[String], String](path, throwable)))

          res <- ZIO.fromEither(loop(config, path.toList, Nil)).map(t => ConfigValue(t))
        } yield res
      },
      List("typesafe-config-hoccon")
    )

  private def asListOfString[A](jlist: => java.util.List[A]): Try[List[String]] =
    Try(jlist).map(_.asScala.toList.map(_.toString))
}
