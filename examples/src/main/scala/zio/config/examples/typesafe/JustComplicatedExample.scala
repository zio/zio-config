package zio.config.examples.typesafe

import zio.{ DefaultRuntime, IO }
import zio.config.typesafe.TypeSafeConfigSource._
import zio.config._
import zio.config.magnolia.ConfigDescriptorProvider.description

object JustComplicatedExample extends App {
  val runtime = new DefaultRuntime {}
  def run[K, A](c: IO[ReadErrorsVector[K], A]): A =
    runtime.unsafeRun(c)

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
      |
      |""".stripMargin

  // Just intentionally complicated example
  final case class Port(va: String)
  final case class Database(port: Port)
  final case class MoreDetail(
    ci: String,
    vi: Either[Int, Either[Long, Either[Double, Either[Float, String]]]],
    lst: List[Int],
    vvv: Option[List[String]]
  )
  final case class DbDetails(hi: String, bi: String, r: List[MoreDetail])
  final case class TableColumns(table: String, columns: List[String], extraDetails: List[DbDetails])
  final case class ExportDetails(exportDetails: List[TableColumns], database: Database)

  val zioConfigResult =
    run(read(description[ExportDetails] from hocon(Right(configString))))

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

  val zioConfigWithKeysInKebabResult =
    run(
      read(description[ExportDetails].convertKey(KeyConversion.camelToKebab) from hocon(Right(kebabCaseConfig)))
    )

  assert(zioConfigWithKeysInKebabResult ==
    ExportDetails(
      List(
        TableColumns(
          "some_name",
          List("a", "b", "c", "d"),
          List(DbDetails("di","ci",
            List(
              MoreDetail("ki",Right(Right(Right(Right("bi")))), List(1, 1, 1),None),
              MoreDetail("ki",Right(Right(Left(1.0882121))), List(1, 2, 1),None),
              MoreDetail("ki",Left(3),List(1, 3, 5),Some(List("1", "2", "3"))))
          ))
        )
      ),
      Database(Port("ba"))
    )
  )

  assert(zioConfigWithKeysInKebabResult == zioConfigResult)
}
