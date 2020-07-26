package zio.config.typesafe

import zio.config.magnolia.DeriveConfigDescriptor.descriptor
import zio.config.read

object TypesafeConfigTestSupport extends EitherSupport {
  final case class Y(z: String)
  final case class X(y: Y)
  final case class W(x: X)
  final case class D(e: List[Int], vvv: List[Either[Int, String]], y: Option[List[String]], z: List[List[List[String]]])
  final case class C(d: List[D], mmm: List[D])
  final case class B(c: List[C], table: String, columns: List[String], mm: List[C])
  final case class A(b: List[B], x: X, w: W, m: List[B])

  val hocon: String =
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
      |            vvv = [a, 1, c]
      |            z = []
      |          }
      |          {
      |            ci : ki
      |            vi : bi
      |            e: [12,12]
      |            vvv = [d]
      |            y = []
      |            z = [[]]
      |          }
      |          {
      |            ci : ki
      |            vi : bi
      |            e: [14,14]
      |            vvv = []
      |            y = ["a"]
      |            z = [[[]]]
      |          }
      |          {
      |            ci : ki
      |            vi : bi
      |            e: [15,15]
      |            vvv = [f, g]
      |            y = ["", ""]
      |            z = [[["x"]]]
      |          }
      |        ]
      |        mmm = []
      |      }
      |    ]
      |    mm = []
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
      |            z = []
      |          }
      |          {
      |            ci : ki
      |            vi : bi
      |            e: [22, 22]
      |            vvv = [sa, l]
      |            z = []
      |          }
      |          {
      |            ci : ki
      |            vi : bi
      |            e: [23, 23, 23]
      |            vvv = [af, l]
      |            y = []
      |            z = [[]]
      |          }
      |          {
      |            ci : ki
      |            vi : bi
      |            e: [24, 24, 24]
      |            vvv = [l]
      |            y = []
      |            z = [[[]]]
      |          }
      |        ]
      |        mmm = []
      |      }
      |    ]
      |    mm = []
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
      |            z = []
      |          }
      |          {
      |            ci : ki
      |            vi : bi
      |            e: [32, 32]
      |            vvv = [x]
      |            z = []
      |          }
      |          {
      |            ci : ki
      |            vi : bi
      |            e: [33, 33, 33]
      |            vvv = [xx]
      |            z = []
      |          }
      |          {
      |            ci : ki
      |            vi : bi
      |            e: []
      |            vvv = [b]
      |            z = []
      |          }
      |          {
      |            ci : ki
      |            vi : bi
      |            e: [37]
      |            vvv = [e,f,g,h,i]
      |            z = []
      |          }
      |        ]
      |        mmm = []
      |      }
      |    ]
      |    mm = []
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
      |m = []
      |""".stripMargin

  val complexHoconSource = TypesafeConfigSource.fromHoconString(hocon).loadOrThrow

  val complexDescription = descriptor[A]
  val readComplexSource  = read(complexDescription from complexHoconSource).loadOrThrow

  val expectedResult =
    A(
      List(
        B(
          List(
            C(
              List(
                D(List(1, 1, 1), List(Right("a"), Left(1), Right("c")), None, Nil),
                D(List(12, 12), List(Right("d")), Some(Nil), List(Nil)),
                D(List(14, 14), Nil, Some(List("a")), List(List(Nil))),
                D(List(15, 15), List(Right("f"), Right("g")), Some(List("", "")), List(List(List("x"))))
              ),
              Nil
            )
          ),
          "some_name",
          List("aa"),
          Nil
        ),
        B(
          List(
            C(
              List(
                D(List(21, 21), List(Right("af")), None, Nil),
                D(List(22, 22), List(Right("sa"), Right("l")), None, Nil),
                D(List(23, 23, 23), List(Right("af"), Right("l")), Some(Nil), List(Nil)),
                D(List(24, 24, 24), List(Right("l")), Some(Nil), List(List(Nil)))
              ),
              Nil
            )
          ),
          "some_name",
          List("a", "b", "c", "d", "e"),
          Nil
        ),
        B(
          List(
            C(
              List(
                D(List(31, 31), List(Right("bb")), None, Nil),
                D(List(32, 32), List(Right("x")), None, Nil),
                D(List(33, 33, 33), List(Right("xx")), None, Nil),
                D(Nil, List(Right("b")), None, Nil),
                D(List(37), List(Right("e"), Right("f"), Right("g"), Right("h"), Right("i")), None, Nil)
              ),
              Nil
            )
          ),
          "some_name",
          List("a"),
          Nil
        )
      ),
      X(Y("k")),
      W(X(Y("k"))),
      Nil
    )
}
