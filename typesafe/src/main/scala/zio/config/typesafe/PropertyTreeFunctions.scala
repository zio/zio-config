package zio.config.typesafe

import com.typesafe.config._
import zio.config.PropertyTree
import zio.config.PropertyTree.{Leaf, Record, Sequence}

import scala.collection.JavaConverters._
import scala.collection.immutable.Nil

// Fixme
private[typesafe] trait PropertyTreeFunctions {
  def treeToTypesafeConfig(
    tree: PropertyTree[String, String]
  ): com.typesafe.config.ConfigObject = {
    println(tree)
    def loop(tree: PropertyTree[String, String], keys: Vector[String]): com.typesafe.config.ConfigObject =
      tree match {
        case Leaf(value) =>
          keys.lastOption.fold(ConfigFactory.empty())(
            last => ConfigFactory.empty().withValue(last, ConfigValueFactory.fromAnyRef(value))
          ).root()

        case Record(value) =>
          value.toList.foldLeft(ConfigFactory.empty().root()) {
            case (acc, (k, tree)) =>
              val newObject =
                loop(tree, Vector(k))

              acc.withValue(k, newObject.getOrDefault(k, newObject))
          }

        case PropertyTree.Empty => ConfigFactory.empty().root()

        case Sequence(values) =>
          keys.headOption match {
            case Some(head) =>
              val r = partitionWith(values) {
                case Leaf(value) => Leaf(value)
              }

              if (r.nonEmpty)
                ConfigFactory.empty().withValue(head, ConfigValueFactory.fromIterable(r.map(_.value).asJava)).root()

              else {
                ConfigFactory
                  .empty()
                  .withValue(
                    head,
                    ConfigValueFactory.fromIterable(values.map(loop(_, keys.tail)).asJava)
                  )
                  .root()
              }


            case None => ConfigFactory.empty().root()
          }
      }

    loop(tree, Vector.empty)
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
