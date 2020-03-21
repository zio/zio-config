package zio.config.examples.typesafe

import zio.config.typesafe.TypeSafeConfigSource._
import zio.config._
import zio.config.magnolia.ConfigDescriptorProvider.description

object JustComplicatedExample extends App {
  val configString =
    """
      |b = [
      |  {
      |   table          : some_name
      |   columns        : [a, b]
      |    c = [
      |      {
      |        d = [
      |          {
      |            ci : ki
      |            vi : bi
      |            e: [1, 1, 1]
      |            vvv = [a, b, c]
      |          }
      |          {
      |            ci : ki
      |            vi : bi
      |            e: [12,12]
      |            vvv = [d]
      |          }
      |          {
      |            ci : ki
      |            vi : bi
      |            e: [13, 13, 13]
      |            vvv = []
      |          }
      |          {
      |            ci : ki
      |            vi : bi
      |            e: [14,14]
      |            vvv = [e]
      |          }
      |          {
      |            ci : ki
      |            vi : bi
      |            e: [15,15]
      |            vvv = [f, g]
      |          }
      |        ]
      |      }
      |    ]
      |  }
      |  {
      |   table          : some_name
      |   columns        : [a, b, c, d, e]
      |    c = [
      |      {
      |        d = [
      |          {
      |            ci : ki
      |            vi : bi
      |            e: [21, 21]
      |            vvv = [af]
      |          }
      |          {
      |            ci : ki
      |            vi : bi
      |            e: [22, 22]
      |            vvv = [sa, l]
      |          }
      |          {
      |            ci : ki
      |            vi : bi
      |            e: [23, 23, 23]
      |            vvv = [af, l]
      |          }
      |          {
      |            ci : ki
      |            vi : bi
      |            e: [24, 24, 24]
      |            vvv = [l]
      |          }
      |          {
      |            ci : ki
      |            vi : bi
      |            e: []
      |            vvv = []
      |          }
      |          {
      |            ci : ki
      |            vi : bi
      |            e: []
      |            vvv = [af]
      |          }
      |          {
      |            ci : ki
      |            vi : bi
      |            e: [27]
      |            vvv = [af]
      |          }
      |        ]
      |      }
      |    ]
      |  }
      |   {
      |    table          : some_name
      |    columns        : []
      |    c = [
      |      {
      |        d = [
      |          {
      |            ci : ki
      |            vi : bi
      |            e: [31, 31]
      |            vvv = []
      |          }
      |          {
      |            ci : ki
      |            vi : bi
      |            e: [32, 32]
      |            vvv = []
      |          }
      |          {
      |            ci : ki
      |            vi : bi
      |            e: [33, 33, 33]
      |            vvv = []
      |          }
      |          {
      |            ci : ki
      |            vi : bi
      |            e: [34, 34, 34]
      |            vvv = [a]
      |          }
      |          {
      |            ci : ki
      |            vi : bi
      |            e: []
      |            vvv = [b]
      |          }
      |          {
      |            ci : ki
      |            vi : bi
      |            e: []
      |            vvv = [c, d]
      |          }
      |          {
      |            ci : ki
      |            vi : bi
      |            e: [37]
      |            vvv = [e,f,g,h,i]
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
  final case class D(e: List[Int], vvv: List[String])
  final case class C(d: List[D])
  final case class B(c: List[C], table: String, columns: List[String])
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
                  D(List(1, 1, 1), List("a", "b", "c")),
                  D(List(12, 12), List("d")),
                  D(List(13, 13, 13), List()),
                  D(List(14, 14), List("e")),
                  D(List(15, 15), List("f", "g"))
                )
              )
            ),
            "some_name",
            List("a", "b")
          ),
          B(
            List(
              C(
                List(
                  D(List(21, 21), List("af")),
                  D(List(22, 22), List("sa", "l")),
                  D(List(23, 23, 23), List("af", "l")),
                  D(List(24, 24, 24), List("l")),
                  D(List(), List()),
                  D(List(), List("af")),
                  D(List(27), List("af"))
                )
              )
            ),
            "some_name",
            List("a", "b", "c", "d", "e")
          ),
          B(
            List(
              C(
                List(
                  D(List(31, 31), List()),
                  D(List(32, 32), List()),
                  D(List(33, 33, 33), List()),
                  D(List(34, 34, 34), List("a")),
                  D(List(), List("b")),
                  D(List(), List("c", "d")),
                  D(List(37), List("e", "f", "g", "h", "i"))
                )
              )
            ),
            "some_name",
            Nil
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
