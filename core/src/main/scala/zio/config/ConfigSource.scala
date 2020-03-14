package zio.config

import java.{ util => ju }
import zio.UIO

import scala.collection.JavaConverters._
import scala.collection.immutable.Nil
import zio.config.PropertyTree.unflatten

final case class ConfigSource[K, V](
  getConfigValue: Vector[K] => Option[PropertyTree[K, V]],
  sourceDescription: List[String]
) { self =>
  final def orElse(that: => ConfigSource[K, V]): ConfigSource[K, V] =
    ConfigSource(
      k => getConfigValue(k).orElse(that.getConfigValue(k)),
      self.sourceDescription ++ that.sourceDescription
    )

  final def <>(that: => ConfigSource[K, V]): ConfigSource[K, V] =
    self orElse that
}

object ConfigSource {
  val SystemEnvironment = "system environment"
  val SystemProperties  = "system properties"
  val JavaProperties    = "java properties"
  val ConstantMap       = "constant <map>"
  val EmptySource       = "<empty>"

  def empty[K, V]: ConfigSource[K, V] =
    ConfigSource(_ => None, EmptySource :: Nil)

  def fromEnv(valueSeparator: Option[Char] = None): UIO[ConfigSource[String, String]] =
    UIO
      .effectTotal(sys.env)
      .map(map => PropertyTree.fromStringMap(map, '_', valueSeparator.getOrElse(':')))
      .map(list => getConfigSource(list, SystemEnvironment))

  def fromProperty(valueSeparator: Option[Char] = None): UIO[ConfigSource[String, String]] =
    for {
      systemProperties <- UIO.effectTotal(java.lang.System.getProperties)
    } yield getConfigSource(
      getPropertyTreeFromJavaPropertes(systemProperties, '.', valueSeparator.getOrElse(':')),
      SystemProperties
    )

  def fromJavaProperties(
    property: ju.Properties,
    valueSeparator: Option[Char] = None
  ): ConfigSource[String, String] =
    getConfigSource(getPropertyTreeFromJavaPropertes(property, '.', valueSeparator.getOrElse(':')), JavaProperties)

  def getPropertyTreeFromJavaPropertes(
    properties: ju.Properties,
    keySeparator: Char,
    valueSeparator: Char
  ): List[PropertyTree[String, String]] = {
    val mapString = properties.stringPropertyNames().asScala.foldLeft(Map.empty[String, String]) { (acc, a) =>
      acc.updated(a, properties.getProperty(a))
    }

    PropertyTree.fromStringMap(mapString, keySeparator, valueSeparator)
  }

  def fromMap(
    map: Map[String, String],
    pathDelimiter: Char = '.',
    valueDelimter: Char = ','
  ): ConfigSource[String, String] =
    fromMapA(map)(x => { val s = x.split(valueDelimter).toList; ::(s.head, s.tail) }, pathDelimiter)

  def fromMultiMap(map: Map[String, ::[String]], pathDelimiter: Char = '.'): ConfigSource[String, String] =
    fromMapA(map)(identity, pathDelimiter)

  private def fromMapA[A, B](map: Map[String, A])(f: A => ::[B], pathDelimiter: Char): ConfigSource[String, B] =
    getConfigSource(
      unflatten(
        map.map(
          tuple =>
            tuple._1.split(pathDelimiter).toVector.filterNot(_.trim == "") ->
              f(tuple._2)
        )
      ),
      ConstantMap
    )

  def getConfigSource[B](
    list: List[PropertyTree[String, B]],
    sourceName: String
  ): ConfigSource[String, B] =
    ConfigSource(
      (path: Vector[String]) => {
        list
          .map(tree => Some(tree.getPath(path.toList)))
          .collectFirst {
            case Some(tree) =>
              //  println(s"for the path ${path}. ${tree}")

              tree
          }
      },
      sourceName :: Nil
    )

  def fromPropertyTree(
    propertyTree: PropertyTree[String, String],
    delimiter: String = "."
  ): ConfigSource[String, String] =
    fromMultiMap(propertyTree.flattenString(delimiter))
}
