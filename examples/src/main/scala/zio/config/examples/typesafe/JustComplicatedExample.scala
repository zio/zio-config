package zio.config.examples.typesafe

import zio.config.typesafe.TypeSafeConfigSource._
import zio.config._
import zio.config.magnolia.ConfigDescriptorProvider.description

object JustComplicatedExample extends App {
  val configString =
    """
      |exportDetails = [
      |  {
      |    table          : some_name
      |    columns        : [ a, b, c, d ]
      |    extraDetails = [
      |      {
      |        hi : di
      |        bi : ci
      |        r = [
      |          {
      |            ci : ki
      |            vi : bi
      |            lst: [1, 1, 1]
      |            vvv = [af, sa, l]
      |
      |          }
      |          {
      |            ci : ki
      |            vi : 1.0882121
      |            lst: [1, 2, 1]
      |            vvv = [t, h, j]
      |
      |          }
      |           {
      |            ci : ki
      |            vi : 3
      |            lst: [1, 3, 5]
      |            vvv = [k, a, d]
      |          }
      |        ]
      |      }
      |    ]
      |  }
      |  {
      |    table          : some_name
      |    columns        : [ x, x, c, d ]
      |    extraDetails = [
      |      {
      |        hi : di
      |        bi : ci
      |        r = [
      |          {
      |            ci : ki
      |            vi : bi
      |            lst: [1, 1, 1]
      |            vvv = [af, sa, 1]
      |
      |          }
      |          {
      |            ci : ki
      |            vi : 1.0882121
      |            lst: [1, 2, 1]
      |            vvv = [5, t, j]
      |
      |          }
      |           {
      |            ci : ki
      |            vi : 3
      |            lst: [1, 3, 5]
      |            vvv = [k, d, j]
      |          }
      |        ]
      |      }
      |    ]
      |  }
      |]
      |
      |""".stripMargin

  com.typesafe.config.ConfigFactory.parseString(configString)

  // Just intentionally complicated example
  final case class Port(va: String)
  final case class Database(port: Port)
  final case class MoreDetail(
    ci: String,
    vi: String
    /*    lst: List[Int],
    vvv: Option[List[String]]*/
  )
  final case class DbDetails(hi: String, bi: String, r: List[MoreDetail])
  final case class TableColumns(table: String, columns: List[String], extraDetails: List[DbDetails])
  final case class ExportDetails(exportDetails: List[TableColumns])

  val zioConfigResult =
    read(description[ExportDetails] from hocon(Right(configString)))

  println(zioConfigResult)

  val kebabCaseConfig =
    """
      |export-details = [
      |  {
      |    table          : some_name
      |    columns        : [ a, b, c, d ]
      |    extra-details = [
      |      {
      |        hi : di
      |        bi : ci
      |        r = [
      |          {
      |            ci : ki
      |            vi : bi
      |            lst: [1, 1, 1]
      |          }
      |          {
      |            ci : ki
      |            vi : 1.0882121
      |            lst: [1, 2, 1]
      |          }
      |           {
      |            ci : ki
      |            vi : 3
      |            lst: [1, 3, 5]
      |            vvv = [1, 2, 3]
      |          }
      |        ]
      |      }
      |    ]
      |  }
      |]
      |
      |database {
      |  port {
      |    va : ba
      |  }
      |}
      |""".stripMargin

  /*  val zioConfigWithKeysInKebabResult =
    read(description[ExportDetails].mapKey(KeyConversion.camelToKebab) from hocon(Right(kebabCaseConfig)))*/

  /*  assert(
    zioConfigWithKeysInKebabResult ==
      Right(
        ExportDetails(
          List(
            TableColumns(
              "some_name",
              List("a", "b", "c", "d"),
              List(
                DbDetails(
                  "di",
                  "ci",
                  List(
                    MoreDetail("ki", Right(Right(Right(Right("bi")))), List(1, 1, 1), None),
                    MoreDetail("ki", Right(Right(Left(1.0882121))), List(1, 2, 1), None),
                    MoreDetail("ki", Left(3), List(1, 3, 5), Some(List("1", "2", "3")))
                  )
                )
              )
            )
          ),
          Database(Port("ba"))
        )
      )
  )

  assert(zioConfigWithKeysInKebabResult == zioConfigResult)*/
}
