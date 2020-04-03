package zio.config.typesafe

import com.typesafe.config.{ ConfigFactory, ConfigValueFactory }
import zio.config.PropertyTree
import zio.config.PropertyTree.{ Leaf, Record, Sequence }
import scala.collection.JavaConverters._

import scala.collection.immutable.Nil

trait PropertyTreeFunctions {
  def treeToTypesafeConfig(
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

              if (keys.isEmpty) {
                acc.withFallback(nextConfig.toConfig)
              } else {
                acc.withFallback(acc.withValue(keys.head, nextConfig))
              }
          }
        case PropertyTree.Empty => ConfigFactory.empty()
        case Sequence(values) =>
          val result =
            keys.headOption match {
              case Some(head) =>
                val r = partitionWith(values) {
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
  )(pf: PartialFunction[PropertyTree[K, V], A]): List[A] =
    trees.map {
      case tree if pf.isDefinedAt(tree) => pf(tree) :: Nil
      case _                            => Nil
    }.foldLeft(List.empty[A]) {
      case (accLeft, left) => (accLeft ++ left)
    }
}
