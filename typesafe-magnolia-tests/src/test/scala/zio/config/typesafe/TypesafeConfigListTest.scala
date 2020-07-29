package zio.config.typesafe

import zio.config._
import zio.config.magnolia.DeriveConfigDescriptor.descriptor
import zio.test.Assertion._
import zio.test.{ DefaultRunnableSpec, suite, test, _ }

object TypesafeConfigListTest extends DefaultRunnableSpec {

  val spec = suite("TypesafeConfig List")(
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

      val zioConfigWithKeysInKebabResult = TypesafeConfigSource
        .fromHoconString(kebabCaseConfig)
        .fold(v => Left(v), s => read(descriptor[ExportDetails].mapKey(toKebabCase) from s))

      val expectedResult = Right(
        ExportDetails(
          List(
            Details1(
              "some_name",
              List("a", "b", "c", "d"),
              List(
                Details2(
                  "di",
                  "ci",
                  List(
                    Details3("ki", Right(Right(Right(Right("bi")))), List(1, 1, 1), Some(Nil)),
                    Details3("ki", Right(Right(Left(1.0882121))), List(1, 2, 1), None),
                    Details3("ki", Left(3), List(1, 3, 5), Some(List(1, 2, 3)))
                  )
                ),
                Details2(
                  "di",
                  "ci",
                  List(
                    Details3("ki", Right(Right(Right(Right("bi")))), List(1, 1, 1), Some(Nil)),
                    Details3("ki", Right(Right(Left(1.0882121))), List(1, 2, 1), None),
                    Details3("ki", Left(3), List(1, 3, 5), Some(List(1, 2, 3)))
                  )
                ),
                Details2("di", "ci", Nil)
              )
            ),
            Details1(
              "some_name1",
              Nil,
              List(Details2("di", "ci", Nil), Details2("di", "ci", Nil), Details2("di", "ci", Nil))
            ),
            Details1("some_name1", Nil, Nil),
            Details1("some_name2", Nil, Nil),
            Details1("some_name2", Nil, Nil)
          ),
          Database(Port("ba"))
        )
      )

      assert(zioConfigWithKeysInKebabResult)(equalTo(expectedResult))
    }
  )
}
