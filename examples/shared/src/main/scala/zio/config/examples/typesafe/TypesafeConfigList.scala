package zio.config.examples.typesafe

import zio.config._
import zio.config.examples.ZioOps
import zio.{Config, ConfigProvider, IO}

import magnolia._

object TypesafeConfigList extends App with EitherImpureOps {
  val configString: String =
    """
      |b = [
      |  {
      |   table          : some_name
      |   columns        : [aa]
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
      |
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
      |        ]
      |      }
      |    ]
      |  }
      |   {
      |    table          : some_name
      |    columns        : [a]
      |    c = [
      |      {
      |        d = [
      |          {
      |            ci : ki
      |            vi : bi
      |            e: [31, 31]
      |            vvv = [bb]
      |          }
      |          {
      |            ci : ki
      |            vi : bi
      |            e: [32, 32]
      |            vvv = [x]
      |          }
      |          {
      |            ci : ki
      |            vi : bi
      |            e: [33, 33, 33]
      |            vvv = [xx]
      |          }
      |          {
      |            ci : ki
      |            vi : bi
      |            e: [31]
      |            vvv = [b]
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

  val expectedResult: A =
    A(
      List(
        B(
          List(
            C(
              List(
                // NonEmptyList is simply scala.:: which is a List. However, if the list was empty you get a error
                D(List(14, 14), List("e")),
                D(List(12, 12), List("d")),
                D(List(1, 1, 1), List("a", "b", "c")),
                D(List(15, 15), List("f", "g"))
              )
            )
          ),
          "some_name",
          List("aa")
        ),
        B(
          List(
            C(
              List(
                D(List(21, 21), List("af")),
                D(List(24, 24, 24), List("l")),
                D(List(22, 22), List("sa", "l")),
                D(List(23, 23, 23), List("af", "l"))
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
                D(List(37), List("e", "f", "g", "h", "i")),
                D(List(33, 33, 33), List("xx")),
                D(List(32, 32), List("x")),
                D(List(31), List("b")),
                D(List(31, 31), List("bb"))
              )
            )
          ),
          "some_name",
          List("a")
        )
      ),
      X(Y("k")),
      W(X(Y("k")))
    )

  import zio.config.typesafe._

  // Since we already have a string with us, we don't need Config Service (or ZIO)
  val source: ConfigProvider =
    ConfigProvider.fromHoconString(configString)

  val zioConfigResult: IO[Config.Error, A] =
    read(deriveConfig[A] from source)

  assert(zioConfigResult equalM expectedResult)

}
