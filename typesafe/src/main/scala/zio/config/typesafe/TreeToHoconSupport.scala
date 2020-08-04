package zio.config.typesafe

import com.typesafe.config._
import zio.config.PropertyTree
import zio.config.PropertyTree.{ Leaf, Record, Sequence }

import scala.collection.JavaConverters._
import scala.collection.immutable.Nil

private[typesafe] trait TreeToHoconSupport {
  def propertyTreeToTypesafeConfig(
    tree: PropertyTree[String, String]
  ): com.typesafe.config.ConfigObject =
    loopAny(tree, None)

  def loopAny(tree: PropertyTree[String, String], key: Option[String]): com.typesafe.config.ConfigObject =
    tree match {
      case leaf @ Leaf(_)     => loopLeaf(key, leaf)
      case record @ Record(_) => loopRecord(key, record)
      case PropertyTree.Empty => loopEmpty(key)
      case seqq @ Sequence(_) => loopSequence(key, seqq)
    }

  def loopEmpty(key: Option[String]): ConfigObject =
    key.fold(ConfigFactory.empty().root()) { k =>
      ConfigFactory.empty().withValue(k, ConfigValueFactory.fromAnyRef(null)).root()
    }

  def loopLeaf(key: Option[String], tree: Leaf[String]): ConfigObject =
    key
      .fold(ConfigFactory.empty())(
        last => ConfigFactory.empty().withValue(last, ConfigValueFactory.fromAnyRef(tree.value))
      )
      .root()

  def loopRecord(key: Option[String], tree: Record[String, String]): ConfigObject = {
    val inner = tree.value.toList.foldLeft(ConfigFactory.empty().root()) {
      case (acc, (k, tree)) =>
        val newObject =
          loopAny(tree, Some(k))

        acc.withValue(k, newObject.getOrDefault(k, newObject))
    }
    key.fold(inner)(path => inner.atPath(path).root())
  }

  def loopSequence(key: Option[String], tree: Sequence[String, String]): ConfigObject =
    key match {
      case Some(key) =>
        val leaves = partitionWith(tree.value) {
          case Leaf(value) => Leaf(value)
        }

        if (leaves.nonEmpty)
          ConfigFactory.empty().withValue(key, ConfigValueFactory.fromIterable(leaves.map(_.value).asJava)).root()
        else {
          ConfigFactory
            .empty()
            .withValue(
              key,
              ConfigValueFactory.fromIterable(
                tree.value
                  .map({ t =>
                    val newObj = loopAny(t, Some(key))
                    newObj.getOrDefault(key, newObj)
                  })
                  .asJava
              )
            )
            .root()
        }

      case None => ConfigFactory.empty().root()
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
