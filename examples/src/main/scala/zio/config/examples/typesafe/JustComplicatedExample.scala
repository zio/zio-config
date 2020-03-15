package zio.config.examples.typesafe

import zio.config.typesafe.TypeSafeConfigSource._
import zio.config._
import zio.config.magnolia.ConfigDescriptorProvider.description

object JustComplicatedExample extends App {
  val configString =
    /*    """
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
      |        ]
      |      }
      |    ]
      |  }
      |  {
      |    table          : some_name
      |    columns        : [ x, x, c, d ]
      |    extraDetails = [
      |      {
      |        hi : di2
      |        bi : ci2
      |        r = [
      |          {
      |            ci : ki2
      |            vi : bi2
      |            lst: [11, 11, 11]
      |            vvv = [af, sa, 1]
      |
      |          }
      |          {
      |            ci : ki22
      |            vi : 5.0882121
      |            lst: [11, 21, 11]
      |            vvv = [5, t, j]
      |
      |          }
      |        ]
      |      }
      |    ]
      |  }
      |]
      |
      |""".stripMargin*/
    """
      |b = [
      |  {
      |   table          : some_name
      |    c = [
      |      {
      |        d = [
      |          {
      |            e: [1, 1, 1]
      |          }
      |          {
      |            e: [12,12]
      |          }
      |          {
      |            e: [13, 13, 13]
      |          }
      |          {
      |            e: [14,14]
      |          }
      |          {
      |            e: [15,15]
      |          }
      |        ]
      |      }
      |    ]
      |  }
      |  {
      |   table          : some_name
      |    c = [
      |      {
      |        d = [
      |          {
      |            e: [21, 21]
      |          }
      |          {
      |            e: [22, 22]
      |          }
      |          {
      |            e: [23, 23, 23]
      |          }
      |          {
      |            e: [24, 24, 24]
      |          }
      |          {
      |            e: []
      |          }
      |          {
      |            e: []
      |          }
      |          {
      |            e: [27]
      |          }
      |        ]
      |      }
      |    ]
      |  }
      |   {
      |    table          : some_name
      |    c = [
      |      {
      |        d = [
      |          {
      |            e: [31, 31]
      |          }
      |          {
      |            e: [32, 32]
      |          }
      |          {
      |            e: [33, 33, 33]
      |          }
      |          {
      |            e: [34, 34, 34]
      |          }
      |          {
      |            e: []
      |          }
      |          {
      |            e: []
      |          }
      |          {
      |            e: [37]
      |          }
      |        ]
      |      }
      |    ]
      |  }
      |]
      |
      |x {
      |  y {
      |    z : k
      |  }
      |}
      |
      |w {
      | x {
      |  y {
      |    z : k
      |  }
      | }
      |}
      |
      |""".stripMargin

  // Just intentionally complicated example
  final case class Y(z: String)
  final case class X(y: Y)
  final case class W(x: X)
  final case class D(e: List[Int])
  final case class C(d: List[D])
  final case class B(c: List[C], table: String)
  final case class A(b: List[B], x: X, w: W)

  val zioConfigResult =
    read(description[A] from hocon(Right(configString)))

  println(
    zioConfigResult
  )

  assert(
    zioConfigResult == Right(
      A(
        List(
          B(
            List(
              C(
                List(
                  D(List(1, 1, 1)),
                  D(List(12, 12)),
                  D(List(13, 13, 13)),
                  D(List(14, 14)),
                  D(List(15, 15))
                )
              )
            ),
            "some_name"
          ),
          B(
            List(
              C(
                List(
                  D(List(21, 21)),
                  D(List(22, 22)),
                  D(List(23, 23, 23)),
                  D(List(24, 24, 24)),
                  D(List()),
                  D(List()),
                  D(List(27))
                )
              )
            ),
            "some_name"
          ),
          B(
            List(
              C(
                List(
                  D(List(31, 31)),
                  D(List(32, 32)),
                  D(List(33, 33, 33)),
                  D(List(34, 34, 34)),
                  D(List()),
                  D(List()),
                  D(List(37))
                )
              )
            ),
            "some_name"
          )
        ),
        X(
          Y("k")
        ),
        W(
          X(
            Y("k")
          )
        )
      )
    )
  )

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
