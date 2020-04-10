package zio.config.typesafe

import com.typesafe.config._
import zio.config.PropertyTree
import zio.config.PropertyTree.{ Leaf, Record, Sequence }

import scala.collection.JavaConverters._
import scala.collection.immutable.Nil

private[typesafe] trait PropertyTreeFunctions {
  def loopLeaf(keys: List[String], tree: Leaf[String]): ConfigObject =
    keys.headOption
      .fold(ConfigFactory.empty())(
        last => ConfigFactory.empty().withValue(last, ConfigValueFactory.fromAnyRef(tree.value))
      )
      .root()

  def loopRecord(keys: List[String], tree: Record[String, String]): ConfigObject =
    tree.value.toList.foldLeft(ConfigFactory.empty().root()) {
      case (acc, (k, tree)) =>
        val newObject =
          loopAny(tree, List(k))

        acc.withValue(k, newObject.getOrDefault(k, newObject))
    }

  def loopSequence(keys: List[String], tree: Sequence[String, String]): ConfigObject =
    keys.headOption match {
      case Some(head) =>
        val leaves = partitionWith(tree.value) {
          case Leaf(value) => Leaf(value)
        }

        if (leaves.nonEmpty)
          ConfigFactory.empty().withValue(head, ConfigValueFactory.fromIterable(leaves.map(_.value).asJava)).root()
        else {
          ConfigFactory
            .empty()
            .withValue(
              head,
              ConfigValueFactory.fromIterable(tree.value.map(loopAny(_, keys.tail)).asJava)
            )
            .root()
        }

      case None => ConfigFactory.empty().root()
    }

  def loopAny(tree: PropertyTree[String, String], keys: List[String]): com.typesafe.config.ConfigObject =
    tree match {
      case leaf @ Leaf(_)     => loopLeaf(keys, leaf)
      case record @ Record(_) => loopRecord(keys, record)
      case PropertyTree.Empty => ConfigFactory.empty().root()
      case seqq @ Sequence(_) => loopSequence(keys, seqq)
    }

  def treeToTypesafeConfig(
    tree: PropertyTree[String, String]
  ): com.typesafe.config.ConfigObject =
    loopAny(tree, List.empty)

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
