package zio.config.typesafe

import com.github.ghik.silencer.silent
import com.typesafe.config._
import zio.ZIO
import zio.config.PropertyTree._
import zio.config._

import java.io.File
import java.lang.{Boolean => JBoolean}
import scala.collection.immutable.Nil
import scala.jdk.CollectionConverters._
import scala.util.{Failure, Success, Try}
import zio.ConfigProvider
import com.github.ghik.silencer.silent
import com.typesafe.config._
import zio.ZIO
import zio.config.PropertyTree._
import zio.config._

import java.io.File
import java.lang.{Boolean => JBoolean}
import scala.collection.immutable.Nil
import scala.jdk.CollectionConverters._
import scala.util.{Failure, Success, Try}
import zio.ConfigProvider, ConfigProvider._
import zio.Chunk
import scala.util.Try

import zio.Config

import zio.Trace
import zio.IO
import com.typesafe.config.ConfigValueType.LIST
import com.typesafe.config.ConfigValueType.NUMBER
import com.typesafe.config.ConfigValueType.STRING
import com.typesafe.config.ConfigValueType.OBJECT
import com.typesafe.config.ConfigValueType.BOOLEAN
import com.typesafe.config.ConfigValueType.NULL

@silent("Unused import")
object TypesafeConfigSource {
  import VersionSpecificSupport._

  def fromResourcePath_ : ConfigProvider = null

  /**
   * Retrieve a `ConfigSource` from `typesafe-config` from a given file in resource classpath.
   *
   * A complete example usage:
   *
   * {{{
   *
   *   case class MyConfig(port: Int, url: String)
   *
   *   val result: IO[ReadError[String], MyConfig] =
   *     read(descriptor[MyConfig] from TypesafeConfigSource.fromResourcePath))
   * }}}
   */
  def fromResourcePath: ConfigSource =
    fromTypesafeConfig(
      ZIO.attempt(ConfigFactory.load.resolve)
    )

  /**
   * Retrieve a `ConfigSource` from `typesafe-config` from a given config file
   *
   * A complete example usage:
   *
   * {{{
   *   val configSource = TypesafeConfigSource.fromHoconFile(new File("/path/to/xyz.hocon"))
   *
   *   case class MyConfig(port: Int, url: String)
   *
   *   val result: Task[MyConfig] =
   *     configSource.flatMap(source => ZIO.fromEither(read(descriptor[MyConfig] from source))
   * }}}
   */
  def fromHoconFile[A](file: File): ConfigSource = {
    val rawConfig =
      ZIO
        .attempt(ConfigFactory.parseFile(file).resolve)

    fromTypesafeConfig(rawConfig)
  }

  /**
   * Retrieve a `ConfigSource` from `typesafe-config` from a path to a config file
   *
   * A complete example usage:
   *
   * {{{
   *   val configSource =
   *     TypesafeConfigSource.fromHoconFilePath("/path/to/xyz.hocon")
   *
   *   case class MyConfig(port: Int, url: String)
   *
   *   val result: Task[MyConfig] =
   *     read(descriptor[MyConfig] from configSource)
   * }}}
   */
  def fromHoconFilePath[A](filePath: String): ConfigSource =
    fromHoconFile(new File(filePath))

  /**
   * Retrieve a `ConfigSource` from `typesafe-config` HOCON string.
   *
   * A complete example usage:
   *
   * {{{
   *
   *   val hocon =
   *     s"""
   *       {
   *          port : 8080
   *          url  : abc.com
   *       }
   *
   *     """
   *   val configSource = TypesafeConfigSource.fromHoconString(hocon)
   *
   *   case class MyConfig(port: Int, url: String)
   *
   *   val result: Either[ReadError[String], MyConfig] =
   *     configSource.flatMap(source => read(descriptor[MyConfig] from source)))
   * }}}
   */

  def fromHoconString_(input: String): ConfigProvider =
    fromTypesafeConfig_(ConfigFactory.parseString(input).resolve)

  def fromHoconString(input: String): ConfigSource =
    fromTypesafeConfig(
      ZIO
        .attempt(ConfigFactory.parseString(input).resolve)
    )

  /**
   * Retrieve a `ConfigSource` from `typesafe-config` data type.
   *
   * A complete example usage:
   *
   * {{{
   *
   *   val hocon =
   *     s"""
   *       {
   *          port : 8080
   *          url  : abc.com
   *       }
   *
   *     """
   *   val configSource = TypesafeConfigSource.fromTypesafeConfig(ZIO.attempt(ConfigFactory.load.resolve))
   *
   *   case class MyConfig(port: Int, url: String)
   *
   *   val result: Either[ReadError[String], MyConfig] =
   *     configSource.flatMap(source => read(descriptor[MyConfig] from source)))
   * }}}
   *
   * We are ending up calling `memoize` (without leaving it to the user)
   * is some of th limitations in typesafe/config project such as:
   *
   * https://github.com/lightbend/config/issues/30 (open as of 29/12/2021)
   */
  def fromTypesafeConfig(
    rawConfig: ZIO[Any, Throwable, com.typesafe.config.Config]
  ): ConfigSource = {
    val effect = rawConfig
      .mapError(exception => ReadError.SourceError(message = exception.toString))
      .flatMap { value =>
        getPropertyTree(value) match {
          case Left(error) =>
            ZIO.fail(ReadError.SourceError(message = error))
          case Right(tree) =>
            ZIO.succeed((path: PropertyTreePath[String]) => ZIO.succeed(tree.at(path)))
        }
      }

    ConfigSource.fromManaged("hocon", effect).memoize
  }

  // FIXME: Define KeyComponent and return Map[Chunk[KeyComponent], String]
  // Make Config.Table to work with Chunk[KeyComponent] and allow easier and typesafe access to Index
  // Make Flat and fromFlat to work with Map[Chunk[KeyComponent], String] instead of Map[String, String]
  def fromTypesafeConfig_(config: com.typesafe.config.Config): ConfigProvider = {
    def loop(config: com.typesafe.config.Config): Map[String, String] = {
      val initLevel = config.entrySet.asScala.map(entry => (entry.getKey(), entry.getValue())).toMap
      initLevel.flatMap({ case (k, possibleConfigValue) =>
        possibleConfigValue.valueType() match {
          case LIST    =>
            Try(config.getConfigList(k)) match {
              case Failure(exception) =>
                val result = config.getList(k).unwrapped().asScala.toList
                Map(k -> result.map(_.toString).mkString(","))

              case Success(value)     =>
                val list: List[com.typesafe.config.Config] = value.asScala.toList
                list.zipWithIndex.map { case (config: com.typesafe.config.Config, index: Int) =>
                  loop(config).map { case (newK, v) => Map(List(k, s"[${index}]", newK).mkString(".") -> v) }
                    .reduceOption(_ ++ _)
                    .getOrElse(Map.empty[String, String])
                }.reduceOption(_ ++ _).getOrElse(Map.empty[String, String])

            }
          case NUMBER  => Map(k -> config.getNumber(k).toString())
          case STRING  => Map(k -> config.getString(k))
          case OBJECT  => throw new Exception("It shouldn't happen")      //FIXME: Move to IO
          case BOOLEAN => Map(k -> config.getBoolean(k).toString())
          case NULL    => throw new Exception("Well I can't do anything") // FIXME: Move to IO
        }
      })
    }

    println(loop(config))

    ConfigProvider.fromMap(loop(config))
  }

  /**
   * Get `PropertyTree` from a typesafe Config
   */
  private[config] def getPropertyTree(
    input: com.typesafe.config.Config
  ): Either[String, PropertyTree[String, String]] = {
    val strictLeaf                          = Leaf(_: String, canBeSequence = false)
    def loopBoolean(value: Boolean)         = strictLeaf(value.toString)
    def loopNumber(value: Number)           = strictLeaf(value.toString)
    val loopNull                            = PropertyTree.empty
    def loopString(value: String)           = strictLeaf(value)
    def loopList(values: List[ConfigValue]) = Sequence(values.map(loopAny))

    def loopConfig(config: ConfigObject) =
      Record(config.asScala.toVector.map { case (key, value) =>
        key -> loopAny(value)
      }.toMap)

    def loopAny(value: ConfigValue): PropertyTree[String, String] =
      value.valueType() match {
        case ConfigValueType.OBJECT  =>
          loopConfig(value.asInstanceOf[ConfigObject])
        case ConfigValueType.LIST    =>
          loopList(value.asInstanceOf[ConfigList].asScala.toList)
        case ConfigValueType.BOOLEAN =>
          loopBoolean(value.unwrapped().asInstanceOf[JBoolean])
        case ConfigValueType.NUMBER  =>
          loopNumber(value.unwrapped().asInstanceOf[Number])
        case ConfigValueType.NULL    => loopNull
        case ConfigValueType.STRING  =>
          loopString(value.unwrapped().asInstanceOf[String])
      }

    Try(loopConfig(input.root())) match {
      case Failure(t)     =>
        Left(
          "Unable to form the zio.config.PropertyTree from Hocon string." +
            " This may be due to the presence of explicit usage of nulls in hocon string. " +
            t.getMessage
        )
      case Success(value) => Right(value)
    }
  }

  /**
   * Get a `typesafe.config.ConfigObject` from a `PropertyTree`
   */
  private[config] def fromPropertyTree(
    tree: PropertyTree[String, String]
  ): com.typesafe.config.ConfigObject =
    loopAny(tree, None)

  private def loopAny(tree: PropertyTree[String, String], key: Option[String]): com.typesafe.config.ConfigObject =
    tree match {
      case leaf @ Leaf(_, _)  => loopLeaf(key, leaf)
      case record @ Record(_) => loopRecord(key, record)
      case PropertyTree.Empty => loopEmpty(key)
      case seqq @ Sequence(_) => loopSequence(key, seqq)
    }

  private def loopEmpty(key: Option[String]): ConfigObject =
    key.fold(ConfigFactory.empty().root()) { k =>
      ConfigFactory
        .empty()
        .withValue(k, ConfigValueFactory.fromAnyRef(null))
        .root()
    }

  private def loopLeaf(key: Option[String], tree: Leaf[String]): ConfigObject =
    key
      .fold(ConfigFactory.empty())(last =>
        ConfigFactory
          .empty()
          .withValue(
            s""" "${last}" """,
            ConfigValueFactory.fromAnyRef(tree.value)
          )
      )
      .root()

  private def loopRecord(key: Option[String], tree: Record[String, String]): ConfigObject = {
    val inner = tree.value.toList.foldLeft(ConfigFactory.empty().root()) { case (acc, (k, tree)) =>
      val newObject =
        loopAny(tree, Some(k))

      acc.withValue(k, newObject.getOrDefault(k, newObject))
    }
    key.fold(inner)(key => inner.atKey(key).root())
  }

  private def loopSequence(key: Option[String], tree: Sequence[String, String]): ConfigObject =
    key match {
      case Some(key) =>
        val leaves = partitionWith(tree.value) { case Leaf(value, bool) =>
          Leaf(value, bool)
        }

        if (leaves.nonEmpty)
          ConfigFactory
            .empty()
            .withValue(
              key,
              ConfigValueFactory.fromIterable(leaves.map(_.value).asJava)
            )
            .root()
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
