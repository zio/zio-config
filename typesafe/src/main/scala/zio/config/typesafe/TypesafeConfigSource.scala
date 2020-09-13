package zio.config.typesafe

import java.io.File
import java.lang.{ Boolean => JBoolean }

import com.typesafe.config._
import zio.config.PropertyTree.{ Leaf, _ }
import zio.config.{ ConfigSource, _ }
import zio.{ IO, Task, ZIO }

import scala.collection.JavaConverters._
import scala.util.{ Failure, Success, Try }

object TypesafeConfigSource {
  def fromDefaultLoader: Either[ReadError[String], ConfigSource] =
    fromTypesafeConfig(ConfigFactory.load.resolve)

  def fromHoconFile[A](
    file: File
  ): Task[ConfigSource] =
    IO.effect(ConfigFactory.parseFile(file).resolve)
      .flatMap(typesafeConfig => {
        ZIO
          .fromEither(fromTypesafeConfig(typesafeConfig))
          .mapError(str => new RuntimeException(str.nonPrettyPrintedString))
      })

  def fromHoconString(
    input: String
  ): Either[ReadError[String], zio.config.ConfigSource] =
    fromTypesafeConfig(
      ConfigFactory.parseString(input).resolve
    )

  def fromTypesafeConfig(
    input: => com.typesafe.config.Config
  ): Either[ReadError[String], ConfigSource] =
    Try {
      input
    } match {
      case Failure(exception) => Left(ReadError.SourceError(message = exception.getMessage))
      case Success(value) =>
        getPropertyTree(value) match {
          case Left(value)  => Left(ReadError.SourceError(message = value))
          case Right(value) => Right(ConfigSource.fromPropertyTree(value, "hocon", LeafForSequence.Invalid))
        }
    }

  private[config] def getPropertyTree(
    input: com.typesafe.config.Config
  ): Either[String, PropertyTree[String, String]] = {
    def loopBoolean(value: Boolean)         = Leaf(value.toString)
    def loopNumber(value: Number)           = Leaf(value.toString)
    val loopNull                            = PropertyTree.empty
    def loopString(value: String)           = Leaf(value)
    def loopList(values: List[ConfigValue]) = Sequence(values.map(loopAny))

    def loopConfig(config: ConfigObject) =
      Record(config.asScala.toVector.map { case (key, value) => key -> loopAny(value) }.toMap)

    def loopAny(value: ConfigValue): PropertyTree[String, String] = value.valueType() match {
      case ConfigValueType.OBJECT  => loopConfig(value.asInstanceOf[ConfigObject])
      case ConfigValueType.LIST    => loopList(value.asInstanceOf[ConfigList].asScala.toList)
      case ConfigValueType.BOOLEAN => loopBoolean(value.unwrapped().asInstanceOf[JBoolean])
      case ConfigValueType.NUMBER  => loopNumber(value.unwrapped().asInstanceOf[Number])
      case ConfigValueType.NULL    => loopNull
      case ConfigValueType.STRING  => loopString(value.unwrapped().asInstanceOf[String])
    }

    Try(loopConfig(input.root())) match {
      case Failure(t) =>
        Left(
          "Unable to form the zio.config.PropertyTree from Hocon string." +
            " This may be due to the presence of explicit usage of nulls in hocon string. " +
            t.getMessage
        )
      case Success(value) => Right(value)
    }
  }
}
