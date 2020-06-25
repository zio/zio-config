package zio.config.examples.typesafe

import com.typesafe.config.ConfigRenderOptions
import zio.config.typesafe.TypesafeConfigSource
import zio.config.{ read, write }
import zio.config.ConfigDescriptor._, zio.config.typesafe._

object MapExample extends App with EitherImpureOps {
  final case class A(m1: Map[String, List[Int]], l1: List[Int], l2: List[Int], m2: Map[String, B])

  object A {
    val config: _root_.zio.config.ConfigDescriptor[A] =
      (map("m1")(list(int)) |@| list("l1")(int) |@| list("l2")(int) |@| map("m2")(B.config))(A.apply, A.unapply)
  }

  final case class B(m1: Map[String, C], i: Int)

  object B {
    val config: _root_.zio.config.ConfigDescriptor[B] =
      (map("m1")(C.config) |@| int("ll"))(B.apply, B.unapply)
  }

  final case class C(a1: String, a2: Int)

  object C {
    val config: _root_.zio.config.ConfigDescriptor[C] =
      (string("a1") |@| int("a2"))(C.apply, C.unapply)
  }

  val hocon =
    s"""
       | m1: {
       |    m11  : [1, 2]
       |    m12  : [1]
       |  }
       |
       |  l1 : []
       |
       |  l2: [1, 3, 3]
       |
       |  m2 : {
       |    m21 : {
       |      m1: {
       |        m211 : {
       |          a1 : a1v
       |          a2 : 2
       |        }
       |         m212 : {
       |          a1 : a1v
       |          a2 : 1
       |         }
       |       }
       |       
       |       ll : 1
       |    }
       |
       |    m22 : {
       |      m1 : {
       |        m221 : {
       |          a1 : a1v
       |          a2 : 1
       |        }
       |         m222 : {
       |          a1 : a1v
       |          a2 : 2
       |        }
       |      }
       |      
       |      ll : 1
       |    }
       |  }
       |""".stripMargin

  val source: zio.config.ConfigSource =
    TypesafeConfigSource.fromHoconString(hocon).loadOrThrow

  val readResult =
    read(A.config from source).loadOrThrow

  assert(
    readResult ==
      A(
        Map("m11" -> List(1, 2), "m12" -> List(1)),
        List(),
        List(1, 3, 3),
        Map(
          "m21" -> B(Map("m212" -> C("a1v", 1), "m211" -> C("a1v", 2)), 1),
          "m22" -> B(Map("m221" -> C("a1v", 1), "m222" -> C("a1v", 2)), 1)
        )
      )
  )

  val invalidHocon =
    s"""
       | m1: {
       |    m11  : [1, 2]
       |    m12  : [1]
       |  }
       |
       |  l1 : []
       |
       |  l2: [1, 3, 3]
       |
       |  m2 : {
       |    m21 : {
       |      m1: {
       |        m211 : {
       |          a1 : a1v
       |          a2 : 2
       |        }
       |         m212 : {
       |          a1 : a1v
       |          a2 : 1
       |         }
       |       }
       |    }
       |
       |    m22 : {
       |      m1 : {
       |        m221 : {
       |          a1 : a1v
       |          a2 : 1
       |        }
       |         m222 : {
       |          a1 : a1v
       |        }
       |      }
       |      
       |      ll : 1
       |    }
       |  }
       |""".stripMargin

  // Invalid Hocon for map error reporting
  println(
    TypesafeConfigSource
      .fromHoconString(invalidHocon)
      .flatMap(invalidSource => read(A.config from invalidSource).swap.map(_.prettyPrint()).swap)
  )

  /*
  ╥
  ╠══╦══╗
  ║  ║  ║
  ║  ║  ╠─MissingValue
  ║  ║  ║ path: m2.m21.ll
  ║  ║  ║ Details: value of type int
  ║  ║  ▼
  ║  ║
  ║  ╠══╗
  ║  ║  ║
  ║  ║  ╠─MissingValue
  ║  ║  ║ path: m2.m22.m1.m222.a2
  ║  ║  ║ Details: value of type int
  ║  ║  ▼
  ║  ▼
  ▼)
   */

  println(
    write(A.config from source, readResult).loadOrThrow.toHocon
      .render(ConfigRenderOptions.concise().setFormatted(true))
  )

  /**
   * {{{
   *
   *   {
   *     "l1" : [],
   *     "l2" : [
   *         "1",
   *         "3",
   *         "3"
   *     ],
   *     "m1" : {
   *         "m11" : [
   *             "1",
   *             "2"
   *         ],
   *         "m12" : [
   *             "1"
   *         ]
   *     },
   *     "m2" : {
   *         "m21" : {
   *             "ll" : "1",
   *             "m1" : {
   *                 "m211" : {
   *                     "a1" : "a1v",
   *                     "a2" : "2"
   *                 },
   *                 "m212" : {
   *                     "a1" : "a1v",
   *                     "a2" : "1"
   *                 }
   *             }
   *         },
   *         "m22" : {
   *             "ll" : "1",
   *             "m1" : {
   *                 "m221" : {
   *                     "a1" : "a1v",
   *                     "a2" : "1"
   *                 },
   *                 "m222" : {
   *                     "a1" : "a1v",
   *                     "a2" : "2"
   *                 }
   *             }
   *         }
   *     }
   * }
   * }}}
   */
  // It picks the value corresponding to y in the value of the dynamic map inside s. This is much powerful
  val hocon3 =
    s"""
       |k : {
       |  s : {
       |     dynamicMap : { y : z }
       |  }
       |}
       |""".stripMargin

  val xx = nested("k") { map("s")(string("y")) }

  assert(
    read(xx from TypesafeConfigSource.fromHoconString(hocon3).loadOrThrow) == Right(Map("dynamicMap" -> "z"))
  )

  val hocon4 =
    s"""
       |k : { dynamicMap : { y : z } }
       |""".stripMargin

  val xx2 = nested("k") { map(string("y")) }

  assert(
    read(xx2 from TypesafeConfigSource.fromHoconString(hocon4).loadOrThrow) == Right(Map("dynamicMap" -> "z"))
  )
}
