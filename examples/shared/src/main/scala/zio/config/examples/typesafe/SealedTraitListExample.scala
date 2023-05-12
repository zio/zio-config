package zio.config.examples.typesafe

import zio.config._
import zio.config.derivation.nameWithLabel
import zio.config.examples.ZioOps
import zio.{Config, ConfigProvider, IO}

import typesafe._
import magnolia._

object SealedTraitListExample extends App {

  @nameWithLabel
  sealed trait DataTransformation

  case class CastColumns(dataTypeMapper: Map[String, String]) extends DataTransformation

  case class TransformationRules(transformations: List[DataTransformation])

  val transformations: String =
    s"""
       |transformations = [
       |      {
       |        type = "CastColumns"
       |        dataTypeMapper = {
       |          "col_A" = "string"
       |          "col_B" = "double"
       |          "col_C" = "decimal(19,2)"
       |        }
       |      }
       |    ]
       |""".stripMargin

  val pgm: IO[Config.Error, TransformationRules] =
    ConfigProvider.fromHoconString(transformations).load(deriveConfig[TransformationRules])

  pgm equalM (TransformationRules(
    List(CastColumns(Map("col_C" -> "decimal(19,2)", "col_B" -> "double", "col_A" -> "string")))
  ))

}
