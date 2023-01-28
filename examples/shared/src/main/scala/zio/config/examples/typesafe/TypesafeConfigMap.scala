package zio.config.examples.typesafe

import com.typesafe.config.ConfigRenderOptions
import zio.IO
import zio.{Config, ConfigProvider}, Config._
import zio.config.examples._
import zio.config.typesafe._
import zio.config._

object TypesafeConfigMap extends App with EitherImpureOps {
  final case class A(m1: Map[String, List[Int]], l1: List[Int], l2: List[Int], m2: Map[String, B])

  object A {
    val config: Config[A] =
      (table("m1", listOf(int)) zip listOf("l1", int) zip listOf("l2", int) zip table("m2", B.config)).to[A]
  }

  final case class B(m1: Map[String, C], i: Int)

  object B {
    val config: Config[B] =
      (table("m1", C.config) zip int("ll")).to[B]
  }

  final case class C(a1: String, a2: Int)

  object C {
    val config: Config[C] =
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

  val source: ConfigProvider =
    TypesafeConfigSource.fromHoconString(hocon)

  val readResult: IO[Config.Error, A] =
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

}
