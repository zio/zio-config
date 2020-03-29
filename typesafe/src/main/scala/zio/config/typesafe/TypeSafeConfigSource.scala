package zio.config.typesafe

import com.typesafe.config.ConfigFactory
import zio.config.ConfigSource
import zio.config._
import com.typesafe.config.ConfigValueType
import zio.config.PropertyTree.Leaf

import scala.util.Try
import scala.util.Failure
import scala.util.Success
import scala.collection.JavaConverters._
import PropertyTree._

object TypeSafeConfigSource {
  def fromHoconString(
    input: String
  ): Either[String, ConfigSource[String, String]] =
    Try {
      ConfigFactory.parseString(input).resolve
    } match {
      case Failure(exception) => Left(exception.getMessage)
      case Success(value) =>
        getPropertyTree(value) match {
          case Left(value)  => Left(value)
          case Right(value) => Right(ConfigSource.fromPropertyTree(value, "hocon"))
        }
    }

  def fromTypesafeConfig(
    input: com.typesafe.config.Config
  ): Either[String, ConfigSource[String, String]] =
    getPropertyTree(input) match {
      case Left(value)  => Left(value)
      case Right(value) => Right(ConfigSource.fromPropertyTree(value, "hocon"))
    }

  private def getPropertyTree(
    input: com.typesafe.config.Config
  ): Either[String, PropertyTree[String, String]] = {
    def loop(config: com.typesafe.config.Config): Either[String, Map[String, PropertyTree[String, String]]] = {
      val internal = config.root()
      val keySet   = internal.keySet().asScala

      keySet.toList.foldLeft(
        Right(Map.empty[String, PropertyTree[String, String]]): Either[
          String,
          Map[String, PropertyTree[String, String]]
        ]
      ) { (acc, key) =>
        val typeOfSubConfig = config.getValue(key).valueType()
        typeOfSubConfig match {
          case ConfigValueType.OBJECT =>
            acc match {
              case Left(value) => Left(value)
              case Right(acc) =>
                loop(config.getConfig(key)) match {
                  case Left(value)  => Left(value)
                  case Right(value) => Right(acc.updated(key, Record(value)))
                }
            }

          case ConfigValueType.LIST =>
            Try {
              val list =
                config.getConfigList(key).asScala.toList

              if (list.isEmpty) {
                updateKeyAndTree(acc, key, Right(PropertyTree.empty))
              } else
                updateKeyAndTree(
                  acc,
                  key,
                  seqEither(list.map(eachConfig => loop(eachConfig))) match {
                    case Left(value)  => Left(value)
                    case Right(value) => Right(Sequence(value.map(Record(_))))
                  }
                )

            }.orElse({
                Try {
                  val list = config.getStringList(key).asScala.toList

                  if (list.isEmpty) {
                    updateKeyAndTree(acc, key, Right(PropertyTree.empty))
                  } else
                    updateKeyAndTree(
                      acc,
                      key,
                      Right(
                        Sequence(
                          config
                            .getStringList(key)
                            .asScala
                            .map(t => Leaf(t))
                            .toList
                        )
                      )
                    )
                }
              })
              .orElse({
                Try {
                  val list =
                    config.getObjectList(key).asScala.toList

                  if (list.isEmpty)
                    updateKeyAndTree(acc, key, Right(PropertyTree.empty))
                  else
                    updateKeyAndTree(
                      acc,
                      key,
                      seqEither(list.map(eachConfig => loop(eachConfig.toConfig))) match {
                        case Left(value)  => Left(value)
                        case Right(value) => Right(Sequence(value.map(Record(_))))
                      }
                    )
                }
              }) match {
              case Failure(_) =>
                Left(
                  "Unable to form the zio.config.PropertyTree from Hocon string. This may be due to the presence of explicit usage of nulls in hocon string."
                )
              case Success(value) => value
            }

          case ConfigValueType.BOOLEAN =>
            updateKeyAndTree(acc, key, Right(Leaf(config.getBoolean(key).toString)))
          case ConfigValueType.NUMBER =>
            updateKeyAndTree(acc, key, Right(Leaf(config.getNumber(key).toString)))
          case ConfigValueType.NULL =>
            updateKeyAndTree(acc, key, Right(PropertyTree.empty))
          case ConfigValueType.STRING =>
            updateKeyAndTree(acc, key, Right(Leaf(config.getString(key))))
        }
      }
    }

    loop(input) match {
      case Left(value)  => Left(value)
      case Right(value) => Right(Record(value))
    }
  }

  private def updateKeyAndTree(
    either: Either[String, Map[String, PropertyTree[String, String]]],
    key: String,
    p: Either[String, PropertyTree[String, String]]
  ): Either[String, Map[String, PropertyTree[String, String]]] =
    either match {
      case Left(value) => Left(value)
      case Right(acc) =>
        p match {
          case Left(value)  => Left(value)
          case Right(value) => Right(acc.updated(key, value))
        }
    }
}
