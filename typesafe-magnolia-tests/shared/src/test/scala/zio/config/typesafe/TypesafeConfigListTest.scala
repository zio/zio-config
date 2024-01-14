package zio.config.typesafe

import zio.Config
import zio.config._
import zio.test.Assertion._
import zio.test.{ZIOSpecDefault, _}

import magnolia._

object TypesafeConfigListTest extends ZIOSpecDefault {

  def spec: Spec[Any, Config.Error] = suite("TypesafeConfig List")(
    test("A kebab case for testing HOCON List config") {
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
          |            vvv = []
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
          |
          |      {
          |        hi : di
          |        bi : ci
          |        r = [
          |          {
          |            ci : ki
          |            vi : bi
          |            lst: [1, 1, 1]
          |            vvv = []
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
          |
          |     {
          |        hi : di
          |        bi : ci
          |        r = []
          |      }
          |    ]
          |  }
          |
          |  {
          |    table          : some_name1
          |    columns        : []
          |    extra-details = [
          |      {
          |        hi : di
          |        bi : ci
          |        r = []
          |      }
          |      {
          |        hi : di
          |        bi : ci
          |        r = []
          |      }
          |       {
          |        hi : di
          |        bi : ci
          |        r = []
          |      }
          |    ]
          |  }
          |
          | {
          |    table          : some_name1
          |    columns        : []
          |    extra-details = []
          |  }
          |
          |   {
          |    table          : some_name2
          |    columns        : []
          |    extra-details  = []
          |  }
          |
          |
          |   {
          |    table          : some_name2
          |    columns        : []
          |    extra-details = []
          |  }
          |]
          |
          |database {
          |  port {
          |    va : ba
          |  }
          |}
          |""".stripMargin

      final case class Details3(
        ci: String,
        vi: Either[Int, Either[Long, Either[Double, Either[Float, String]]]],
        lst: List[Int],
        vvv: Option[List[Int]]
      )
      final case class Details2(hi: String, bi: String, r: List[Details3])
      final case class Details1(
        table: String,
        columns: List[String],
        extraDetails: List[Details2]
      )
      final case class ExportDetails(exportDetails: List[Details1], database: Database)
      final case class Port(va: String)
      final case class Database(port: Port)

      val zioConfigWithKeysInKebabResult =
        read(
          deriveConfig[ExportDetails].mapKey(toKebabCase) from TypesafeConfigProvider
            .fromHoconString(kebabCaseConfig)
        )

      val expectedResult =
        ExportDetails(
          exportDetails = List(
            Details1(
              table = "some_name",
              columns = List("a", "b", "c", "d"),
              extraDetails = List(
                Details2(
                  hi = "di",
                  bi = "ci",
                  r = List(
                    Details3(
                      ci = "ki",
                      vi = Right(value = Right(value = Right(value = Right(value = "bi")))),
                      lst = List(1, 1, 1),
                      vvv = Some(Nil)
                    ),
                    Details3(
                      ci = "ki",
                      vi = Right(value = Right(value = Left(value = 1.0882121))),
                      lst = List(1, 2, 1),
                      vvv = None
                    ),
                    Details3(
                      ci = "ki",
                      vi = Left(value = 3),
                      lst = List(1, 3, 5),
                      vvv = Some(List(1, 2, 3))
                    )
                  )
                ),
                Details2(
                  hi = "di",
                  bi = "ci",
                  r = List(
                    Details3(
                      ci = "ki",
                      vi = Right(value = Right(value = Right(value = Right(value = "bi")))),
                      lst = List(1, 1, 1),
                      vvv = Some(Nil)
                    ),
                    Details3(
                      ci = "ki",
                      vi = Right(value = Right(value = Left(value = 1.0882121))),
                      lst = List(1, 2, 1),
                      vvv = None
                    ),
                    Details3(
                      ci = "ki",
                      vi = Left(value = 3),
                      lst = List(1, 3, 5),
                      vvv = Some(List(1, 2, 3))
                    )
                  )
                ),
                Details2(
                  hi = "di",
                  bi = "ci",
                  r = Nil
                )
              )
            ),
            Details1(
              table = "some_name1",
              columns = Nil,
              extraDetails = List(
                Details2(
                  hi = "di",
                  bi = "ci",
                  r = Nil
                ),
                Details2(
                  hi = "di",
                  bi = "ci",
                  r = Nil
                ),
                Details2(
                  hi = "di",
                  bi = "ci",
                  r = Nil
                )
              )
            ),
            Details1(
              table = "some_name1",
              columns = Nil,
              extraDetails = Nil
            ),
            Details1(
              table = "some_name2",
              columns = Nil,
              extraDetails = Nil
            ),
            Details1(
              table = "some_name2",
              columns = Nil,
              extraDetails = Nil
            )
          ),
          database = Database(port = Port(va = "ba"))
        )

      assertZIO(zioConfigWithKeysInKebabResult)(equalTo(expectedResult))
    }
  )
}
