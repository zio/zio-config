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
        def effect[A](f: => A): Either[ReadError.Unknown[Vector[String]], A] =
          Try(f) match {
            case Success(value)     => Right(value)
            case Failure(exception) => Left(ReadError.Unknown(path, exception))
          }

        def getLeafValue(
          config: => com.typesafe.config.Config,
          key: String
        ): Either[ReadError.Unknown[Vector[String]], Option[::[String]]] =
          Try {
            config.getValue(key).valueType()
          } match {
            case Failure(exception) =>
              exception match {
                case _: com.typesafe.config.ConfigException.Missing => Right(None)
                case e                                              => Left(ReadError.Unknown[Vector[String]](path, e))
              }
            case Success(valueType) =>
              if (valueType == ConfigValueType.BOOLEAN) {
                effect(config.getBoolean(key).toString).map(singleton).map(Some(_))
              } else if (valueType == ConfigValueType.NULL) {
                Right(None)
              } else if (valueType == ConfigValueType.NUMBER) {
                effect(config.getNumber(key).toString).map(singleton).map(Some(_))
              } else if (valueType == ConfigValueType.STRING) {
                effect(config.getString(key)).map(singleton).map(Some(_))
              } else if (valueType == ConfigValueType.OBJECT) {
                Left(
                  ReadError.Unknown(
                    path,
                    new RuntimeException(s"The value for the key ${path} is an object and not a primitive.")
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
                      ReadError.Unknown(
                        path,
                        new RuntimeException(
                          "Trying to parse a list of config. However, the type is unidentified. Supports only [list] of [int, boolean, duration, bytes, double, long, memory size]",
                          exception
                        )
                      )
                    )
                  case Success(value) =>
                    value match {
                      case h :: t => Right(Some(::(h, t)))
                      case Nil =>
                        Left(
                          ReadError.Unknown(
                            path,
                            new RuntimeException("List is empty. Only non empty list is supported through zio-config")
                          )
                        )
                    }
                }
              } else {
                Left((ReadError.Unknown(path, new RuntimeException("Unknown type"))))
              }
          }

        def loop(
          parentConfig: com.typesafe.config.Config,
          list: List[String],
          nextPath: List[String]
        ): Either[ReadError.Unknown[Vector[String]], ::[Option[::[String]]]] =
          list match {
            case Nil =>
              nextPath.lastOption match {
                case Some(lastKey) =>
                  getLeafValue(parentConfig, lastKey).map(singleton)

                case None =>
                  Right(singleton(None))
              }

            case head :: next => {
              for {
                res <- Try(parentConfig.getValue(head).valueType()) match {
                        case Failure(_) => Right(::(None: Option[::[String]], Nil))
                        case Success(valueType) =>
                          if (valueType == ConfigValueType.LIST) {
                            for {
                              // A few extra error handling.
                              res <- Try(parentConfig.getConfigList(head).asScala.toList) match {
                                      case Success(allConfigs) =>
                                        val result = seqEither({
                                          allConfigs.toList.map(
                                            eachConfig => loop(eachConfig, next, nextPath :+ head)
                                          )
                                        })

                                        result.map(t => ::(t.flatten.head, t.flatten.tail))

                                      case Failure(_) =>
                                        effect(parentConfig.getList(head).asScala.toList).flatMap {
                                          {
                                            case h :: t
                                                if (::(h, t).forall(
                                                  t =>
                                                    t.valueType() != ConfigValueType.NULL && // A list of list of same type isn't supported
                                                      t.valueType() != ConfigValueType.LIST &&
                                                      t.valueType() != ConfigValueType.OBJECT
                                                )) =>
                                              Right(
                                                singleton(
                                                  Some(::(h.unwrapped.toString, t.map(_.unwrapped.toString))): Option[
                                                    ::[String]
                                                  ]
                                                )
                                              )

                                            case _ =>
                                              Left(
                                                ReadError.Unknown[Vector[String]](
                                                  path,
                                                  new RuntimeException(
                                                    s"The key ${head} is a list, however, it consist of complex types that aren't supported yet in zio-config. Make sure we don't have values such as List(List..) of same types) in hocon"
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
                              Right(singleton(None: Option[::[String]]))
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
                     .mapError(throwable => ReadError.Unknown[Vector[String]](path, throwable))

          res <- ZIO.fromEither(loop(config, path.toList, Nil)).map(t => ConfigValue(t))

        } yield Some(res)
      },
      List("typesafe-config-hocon")
    )

  private def asListOfString[A](jlist: => java.util.List[A]): Try[List[String]] =
    Try(jlist).map(_.asScala.toList.map(_.toString))
}
