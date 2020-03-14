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
  def hocon(input: Either[com.typesafe.config.Config, String]): ConfigSource[String, String] = {
    val tree        = getPropertyTree(input)
    val anotherTree = ConfigSource.getConfigSource(List(tree), "hocon")
    anotherTree
  }

  private def getPropertyTree(input: Either[com.typesafe.config.Config, String]): PropertyTree[String, String] = {
    def loop(config: com.typesafe.config.Config): Map[String, PropertyTree[String, String]] = {
      val internal = config.root()
      val keySet   = internal.keySet().asScala

      keySet.toList.foldLeft(Map.empty[String, PropertyTree[String, String]]) { (acc, key) =>
        {
          val typeOfSubConfig = config.getValue(key).valueType()
          typeOfSubConfig match {
            case ConfigValueType.OBJECT =>
              val subConfigList = config.getConfig(key)
              acc.updated(key, Record(loop(subConfigList)))

            case ConfigValueType.LIST =>
              Try {

                acc.updated(
                  key,
                  Sequence(config.getConfigList(key).asScala.toList.map(eachConfig => Record(loop(eachConfig))))
                )

              }.orElse({
                Try {
                  acc.updated(
                    key,
                    Sequence(
                      config
                        .getList(key)
                        .unwrapped()
                        .asScala
                        .map(t => Record(Map(key -> Leaf(t.toString))))
                        .toList
                    )
                  )
                }
              }) match {
                case Failure(exception) => throw new Exception(s"I cant handle this. ${exception}")
                case Success(value)     => value
              }

            case ConfigValueType.BOOLEAN =>
              acc.updated(key, Leaf(config.getBoolean(key).toString))
            case ConfigValueType.NUMBER =>
              acc.updated(key, Leaf(config.getNumber(key).toString))
            case ConfigValueType.NULL =>
              acc.updated(key, PropertyTree.empty)
            case ConfigValueType.STRING =>
              acc.updated(key, Leaf(config.getString(key).toString))
          }
        }
      }
    }

    Record(
      loop(
        input.fold(
          config => config,
          str => ConfigFactory.parseString(str).resolve
        )
      )
    )
  }
}
