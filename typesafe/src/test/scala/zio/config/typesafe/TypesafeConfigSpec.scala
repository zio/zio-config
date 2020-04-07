package zio.config.typesafe

import zio.test._
import zio.test.Assertion._
import zio.config.BaseSpec
import TypesafeConfigSpecUtils._
import zio.config.PropertyTree.{ Leaf, Record, Sequence }
import zio.config._
import zio.config.magnolia.DeriveConfigDescriptor._

object TypesafeConfigSpec
    extends BaseSpec(
      suite("TypesafeConfig")(
        test("Read mixed list") {
          val res =
            TypeSafeConfigSource.fromHoconString(
              """
                |list = [
                |  "a",
                |  {b = "c"}
                |]
                |""".stripMargin
            )

          val expected = Record(Map("list" -> Sequence(List(Leaf("a"), Record(Map("b" -> Leaf("c")))))))

          assert(res.map(_.getConfigValue(Vector.empty)))(isRight(equalTo(expected)))
        },
        testM(
          "Read a complex hocon structure successfully"
        ) {
          //Fixme
          check(Gen.const(configString)) {
            input =>
              {
                val config =
                  TypeSafeConfigSource.fromHoconString(input) match {
                    case Left(value) => Left(value)
                    case Right(value) =>
                      read(descriptor[A] from value) match {
                        case Left(value)  => Left(value.toString)
                        case Right(value) => Right(value)
                      }
                  }

                assert(config)(
                  equalTo(
                    Right(
                      A(
                        List(
                          B(
                            List(
                              C(
                                List(
                                  D(NonEmptyList(1, 1, 1), List("a", "b", "c")),
                                  D(NonEmptyList(12, 12), List("d")),
                                  D(NonEmptyList(14, 14), List("e")),
                                  D(NonEmptyList(15, 15), List("f", "g"))
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
                                  D(List(22, 22), List("sa", "l")),
                                  D(List(23, 23, 23), List("af", "l")),
                                  D(List(24, 24, 24), List("l"))
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
                                  D(List(31, 31), List("bb")),
                                  D(List(32, 32), List("x")),
                                  D(List(33, 33, 33), List("xx")),
                                  D(List(31), List("b")),
                                  D(List(37), List("e", "f", "g", "h", "i"))
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
                    )
                  )
                )
              }
          }
        }
      )
    )

object TypesafeConfigSpecUtils {

  final case class Y(z: String)
  final case class X(y: Y)
  final case class W(x: X)
  final case class D(e: List[Int], vvv: List[String])
  final case class C(d: List[D])
  final case class B(c: List[C], table: String, columns: List[String])
  final case class A(b: List[B], x: X, w: W)

  //Fixme; Make it Gen
  val configString =
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
}
