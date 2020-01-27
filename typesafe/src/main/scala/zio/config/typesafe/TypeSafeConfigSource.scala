package zio.config.typesafe

import com.typesafe.config.ConfigFactory
import zio.config.ConfigSource

import zio.{ ZIO }
import zio.config._
import com.typesafe.config.ConfigValueType
import scala.util.Try
import scala.util.Failure
import scala.util.Success
import scala.collection.JavaConverters._

object TypeSafeConfigSource {
  def hocon(input: Either[com.typesafe.config.Config, String]): ConfigSource[String, String] =
    ConfigSource(
      (path: Vector[String]) => {
        def effect[A](f: => A): Either[ReadErrorsVector[String, String], A] =
          Try(f) match {
            case Success(value)     => Right(value)
            case Failure(exception) => Left(singleton(ReadError.unknownError(path, exception)))
          }

        val getValue: (
          com.typesafe.config.Config,
          ConfigValueType,
          String
        ) => Either[ReadErrorsVector[String, String], ::[String]] =
          (config, valueType, key) =>
            if (valueType == ConfigValueType.BOOLEAN) {
              effect(config.getBoolean(key).toString).map(singleton)
            } else if (valueType == ConfigValueType.NULL) {
              Left(singleton(ReadError.MissingValue(path)))
            } else if (valueType == ConfigValueType.NUMBER) {
              effect(config.getNumber(key).toString).map(singleton)
            } else if (valueType == ConfigValueType.STRING) {
              effect(config.getString(key)).map(singleton)
            } else if (valueType == ConfigValueType.OBJECT) {
              Left(
                singleton(
                  ReadError.Unknown(
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
                      ReadError.Unknown(
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
              Left(singleton(ReadError.Unknown(path, new RuntimeException("Unknown type"))))
            }

        def loop(
          parentConfig: com.typesafe.config.Config,
          list: List[String],
          nextPath: List[String]
        ): Either[ReadErrorsVector[String, String], ::[Option[String]]] =
          list match {
            case Nil =>
              nextPath.lastOption match {
                case Some(lastKey) =>
                  val r = getValue(parentConfig, parentConfig.getValue(lastKey).valueType(), lastKey)
                  r.map(t => {
                    ::(Some(t.head), t.tail.map(Some(_)))
                  })
                case None => Left(singleton(ReadError.missingValue[Vector[String], String](path)))
              }

            case head :: next => {
              for {
                res <- effect(parentConfig.getValue(head).valueType()) match {
                        case Left(_) => Right(::(None, Nil))
                        case Right(valueType) =>
                          if (valueType == ConfigValueType.LIST) {
                            for {
                              // A few extra error handling.
                              res <- effect(parentConfig.getConfigList(head).asScala.toList) match {
                                      case Right(allConfigs) =>
                                        seqEither(
                                          allConfigs.map(eachConfig => loop(eachConfig, next, nextPath :+ head))
                                        ).map(t => t.flatMap(_.toList))
                                          .flatMap({
                                            case Nil =>
                                              Left(singleton(ReadError.missingValue[Vector[String], String](path)))
                                            case h :: t => Right(::(h, t))
                                          })

                                      case Left(_) =>
                                        effect(parentConfig.getList(head).asScala.toList).flatMap {
                                          {
                                            case Nil =>
                                              Left(singleton(ReadError.missingValue[Vector[String], String](path)))

                                            case h :: t
                                                if (::(h, t).forall(
                                                  t =>
                                                    t.valueType() != ConfigValueType.NULL ||
                                                      t.valueType() != ConfigValueType.LIST ||
                                                      t.valueType() != ConfigValueType.OBJECT
                                                )) =>
                                              Right(
                                                ::(
                                                  Some(h.unwrapped().toString),
                                                  t.map(t => Some(t.unwrapped().toString))
                                                )
                                              )

                                            case _ =>
                                              Left(
                                                singleton(
                                                  ReadError.unknownError[Vector[String], String](
                                                    path,
                                                    new RuntimeException(
                                                      s"Wrong types in the list. Identified the value of ${head} in HOCON as a list, however, it should be a list of primitive values. Ex: [1, 2, 3]"
                                                    )
                                                  )
                                                )
                                              )

                                          }
                                        }
                                    }
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
                      }
              } yield res
            }
          }

        for {
          config <- ZIO
                     .effect(
                       input.fold(
                         config => config,
                         str => ConfigFactory.parseString(str).resolve
                       )
                     )
                     .mapError(throwable => singleton(ReadError.unknownError[Vector[String], String](path, throwable)))

          res <- ZIO.fromEither(loop(config, path.toList, Nil)).map(t => ConfigValue(t))
        } yield res
      },
      List("typesafe-config-hocon")
    )

  private def asListOfString[A](jlist: => java.util.List[A]): Try[List[String]] =
    Try(jlist).map(_.asScala.toList.map(_.toString))
}
