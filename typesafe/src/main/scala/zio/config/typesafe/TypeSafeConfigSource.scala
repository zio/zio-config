package zio.config.typesafe

import com.typesafe.config.{ ConfigFactory, ConfigValueFactory, ConfigValueType }
import zio.config.ConfigSource
import zio.config._
import zio.config.PropertyTree.Leaf

import scala.util.Try
import scala.util.Failure
import scala.util.Success
import scala.collection.JavaConverters._
import PropertyTree._
import java.io.File

import zio.IO
import zio.Task
import zio.ZIO

import scala.collection.immutable.Nil

object TypeSafeConfigSource {
  def fromDefaultLoader: Either[String, ConfigSource[String, String]] =
    fromTypesafeConfig(ConfigFactory.load.resolve)

  def fromHoconFile[A](
    file: File
  ): Task[ConfigSource[String, String]] =
    IO.effect(ConfigFactory.parseFile(file).resolve)
      .flatMap(typesafeConfig => {
        ZIO
          .fromEither(fromTypesafeConfig(typesafeConfig))
          .mapError(str => new RuntimeException(str))
      })

  def fromHoconString(
    input: String
  ): Either[String, ConfigSource[String, String]] =
    fromTypesafeConfig(
      ConfigFactory.parseString(input).resolve
    )

  def fromTypesafeConfig(
    input: com.typesafe.config.Config
  ): Either[String, ConfigSource[String, String]] =
    Try {
      input
    } match {
      case Failure(exception) => Left(exception.getMessage)
      case Success(value) =>
        getPropertyTree(value) match {
          case Left(value)  => Left(value)
          case Right(value) => Right(ConfigSource.fromPropertyTree(value, "hocon"))
        }
    }

  private[config] def getPropertyTree(
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

  def writeHocon(
    tree: PropertyTree[String, String]
  ): com.typesafe.config.ConfigObject = {
    def loop(tree: PropertyTree[String, String], keys: Vector[String]): com.typesafe.config.Config =
      tree match {
        case Leaf(value) =>
          keys.lastOption.fold(ConfigFactory.empty())(
            last => ConfigFactory.empty().withValue(last, ConfigValueFactory.fromAnyRef(value))
          )

        case Record(value) =>
          value.toList.foldLeft(ConfigFactory.empty(): com.typesafe.config.Config) {
            case (acc, v) =>
              val path = keys :+ v._1
              val nextConfig =
                keys.toList match {
                  case _ :: t if t.nonEmpty => loop(v._2, path).getObject(keys.tail.mkString("."))
                  case _                    => loop(v._2, path).root()
                }

              if (keys.isEmpty) nextConfig.toConfig else acc.withValue(keys.head, nextConfig)
          }
        case PropertyTree.Empty => ConfigFactory.empty()
        case Sequence(values) =>
          val result =
            keys.headOption match {
              case Some(head) =>
                val (r, _) = partitionWith(values) {
                  case Leaf(value) => Leaf(value)
                }

                if (r.nonEmpty)
                  ConfigFactory.empty().withValue(head, ConfigValueFactory.fromIterable(r.map(_.value).asJava))
                else
                  ConfigFactory
                    .empty()
                    .withValue(
                      head,
                      ConfigValueFactory.fromIterable(values.map(loop(_, Vector.empty).root()).asJava)
                    )
                    .root()
                    .toConfig

              case None => ConfigFactory.empty()
            }

          result
      }

    loop(tree, Vector.empty).root()
  }

  def partitionWith[K, V, A](
    trees: List[PropertyTree[K, V]]
  )(pf: PartialFunction[PropertyTree[K, V], A]): (List[A], List[PropertyTree[K, V]]) =
    trees.map {
      case tree if pf.isDefinedAt(tree) => (pf(tree) :: Nil, Nil)
      case tree                         => (Nil, tree :: Nil)
    }.foldLeft((List.empty[A], List.empty[PropertyTree[K, V]])) {
      case ((accLeft, accRight), (left, right)) => (accLeft ++ left, accRight ++ right)
    }

}

object Main extends App {
  println(
    TypeSafeConfigSource
      .writeHocon(
        Record(
          Map(
            "key1" -> Sequence(
              List(
                Record(Map("key2" -> Leaf("value2"))),
                Record(Map("key3" -> Leaf("value3"))),
                Record(Map("key4" -> Sequence(List(Leaf("value4")))))
              )
            )
          )
        )
      )
      .render()
  )

  println(
    TypeSafeConfigSource
      .writeHocon(
        PropertyTree
          .Record(
            Map(
              "xyz"     -> Leaf("something"),
              "regions" -> PropertyTree.Sequence(List(Leaf("australia"), Leaf("canada"), Leaf("usa")))
            )
          )
      )
      .render()
  )
}
