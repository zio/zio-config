package zio.config.examples.typesafe

import com.typesafe.config.ConfigRenderOptions
import zio.IO
import zio.config.ConfigDescriptor._
import zio.config.examples._
import zio.config.typesafe._
import zio.config.{ConfigDescriptor, ReadError, read, write, _}

object TypesafeConfigMap extends App with EitherImpureOps {
  final case class A(m1: Map[String, List[Int]], l1: List[Int], l2: List[Int], m2: Map[String, B])

  object A {
    val config: ConfigDescriptor[A] =
      (map("m1")(list(int)) zip list("l1")(int) zip list("l2")(int) zip map("m2")(B.config)).to[A]
  }

  final case class B(m1: Map[String, C], i: Int)

  object B {
    val config: ConfigDescriptor[B] =
      (map("m1")(C.config) zip int("ll")).to[B]
  }

  final case class C(a1: String, a2: Int)

  object C {
    val config: ConfigDescriptor[C] =
      (string("a1") zip int("a2")).to[C]
  }

  val hocon: String =
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
    TypesafeConfigSource.fromHoconString(hocon)

  val readResult: IO[ReadError[String], A] =
    read(A.config from source)

  assert(
    readResult equalM
      A(
        Map("m11" -> List(1, 2), "m12" -> List(1)),
        List(),
        List(1, 3, 3),
        Map(
          "m21"   -> B(Map("m212" -> C("a1v", 1), "m211" -> C("a1v", 2)), 1),
          "m22"   -> B(Map("m221" -> C("a1v", 1), "m222" -> C("a1v", 2)), 1)
        )
      )
  )

  val invalidHocon: String =
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
    read(
      A.config from TypesafeConfigSource
        .fromHoconString(invalidHocon)
    ).mapError(_.prettyPrint()).either.unsafeRun
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
    write(A.config from source, readResult.unsafeRun)
      .map(
        _.toHocon
          .render(ConfigRenderOptions.concise().setFormatted(true))
      )
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
  val hocon3: String =
    s"""
       |k : {
       |  s : {
       |     dynamicMap : { y : z }
       |  }
       |}
       |""".stripMargin

  val xx: ConfigDescriptor[Map[String, String]] = nested("k")(map("s")(string("y")))

  assert(
    read(xx from ConfigSource.fromHoconString(hocon3)) equalM Map("dynamicMap" -> "z")
  )

  val hocon4: String =
    s"""
       |k : { dynamicMap : { y : z } }
       |""".stripMargin

  val xx2: zio.config.ConfigDescriptor[Map[String, String]] = nested("k")(map(string("y")))

  assert(
    read(xx2 from ConfigSource.fromHoconString(hocon4)) equalM Map("dynamicMap" -> "z")
  )
}
