package zio.config

import java.{ util => ju }
import zio.UIO

import scala.collection.JavaConverters._
import scala.collection.immutable.Nil
import zio.config.PropertyTree.unflatten

final case class ConfigSource[K, V](
  getConfigValue: Vector[K] => PropertyTree[K, V],
  sourceDescription: List[String]
) { self =>
  final def orElse(that: => ConfigSource[K, V]): ConfigSource[K, V] =
    ConfigSource(
      k => getConfigValue(k).getOrElse(that.getConfigValue(k)),
      self.sourceDescription ++ that.sourceDescription
    )

  final def <>(that: => ConfigSource[K, V]): ConfigSource[K, V] =
    self orElse that
}

object ConfigSource {
  val SystemEnvironment = "system environment"
  val SystemProperties  = "system properties"

  def empty[K, V]: ConfigSource[K, V] =
    ConfigSource(_ => PropertyTree.empty, Nil)

  def fromSystemEnv(valueSeparator: Option[Char] = None): UIO[ConfigSource[String, String]] =
    UIO
      .effectTotal(sys.env)
      .map(map => ConfigSource.fromMap(map, '_', valueSeparator.getOrElse(':'), SystemEnvironment))

  def fromSystemProperties(valueSeparator: Option[Char] = None): UIO[ConfigSource[String, String]] =
    for {
      systemProperties <- UIO.effectTotal(java.lang.System.getProperties)
    } yield fromProperties(
      systemProperties,
      valueSeparator,
      SystemProperties
    )

  def fromProperties(
    property: ju.Properties,
    valueSeparator: Option[Char] = None,
    source: String
  ): ConfigSource[String, String] = {
    val mapString = property.stringPropertyNames().asScala.foldLeft(Map.empty[String, String]) { (acc, a) =>
      acc.updated(a, property.getProperty(a))
    }

    mergeAll(
      PropertyTree
        .fromStringMap(mapString, '.', valueSeparator.getOrElse(':'))
        .map(tree => fromPropertyTree(tree, source))
    )
  }

  def fromMap(
    map: Map[String, String],
    pathDelimiter: Char = '.',
    valueDelimter: Char = ',',
    source: String
  ): ConfigSource[String, String] =
    fromMapInternal(map)(x => { val s = x.split(valueDelimter).toList; ::(s.head, s.tail) }, pathDelimiter, source)

  def fromMultiMap(
    map: Map[String, ::[String]],
    pathDelimiter: Char = '.',
    source: String
  ): ConfigSource[String, String] =
    fromMapInternal(map)(identity, pathDelimiter, source)

  def mergeAll[K, V](sources: Iterable[ConfigSource[K, V]]): ConfigSource[K, V] =
    sources.foldLeft(ConfigSource.empty: ConfigSource[K, V])(_ orElse _)

  def fromPropertyTree[B](
    tree: PropertyTree[String, B],
    sourceName: String
  ): ConfigSource[String, B] =
    ConfigSource(
      (path: Vector[String]) => tree.getPath(path.toList),
      sourceName :: Nil
    )

  def fromPropertyTrees[B](
    trees: Iterable[PropertyTree[String, B]],
    sourceName: String
  ): ConfigSource[String, B] =
    mergeAll(trees.map(fromPropertyTree(_, sourceName)))

  private def fromMapInternal[A, B](
    map: Map[String, A]
  )(f: A => ::[B], pathDelimiter: Char, source: String): ConfigSource[String, B] =
    fromPropertyTrees(
      unflatten(
        map.map(
          tuple =>
            tuple._1.split(pathDelimiter).toVector.filterNot(_.trim == "") ->
              f(tuple._2)
        )
      ),
      source
    )
}
