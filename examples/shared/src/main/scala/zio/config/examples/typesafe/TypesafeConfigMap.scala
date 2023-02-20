package zio.config.examples.typesafe

import com.typesafe.config.ConfigRenderOptions
import zio.Runtime.default
import zio.config._
import zio.config.examples._
import zio.config.typesafe._
import zio.{Config, ConfigProvider, IO, Unsafe}

import Config._

object TypesafeConfigMap extends App with EitherImpureOps {
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

  val map: Map[String, String] =
    Map(
      "m1.m221.a1" -> "bar",
      "m1.m221.a2" -> "1",
      "m1.m222.a1" -> "foo",
      "m1.m222.a2" -> "2",
      "ll"         -> "1"
    )

  val hocon: String =
    s"""
       |
       |      m1 : {
       |        m221 : {
       |          a1 : bar
       |          a2 : 1
       |        }
       |         m222 : {
       |          a1 : foo
       |          a2 : 2
       |        }
       |      }
       |      ll : 1
       |""".stripMargin

  val source: ConfigProvider =
    TypesafeConfigProvider.fromHoconString(hocon)

  val readResult: IO[Config.Error, B] =
    read(B.config from source)

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
//  println(
//    read(
//      A.config from TypesafeConfigSource
//        .fromHoconString(invalidHocon)
//    ).mapError(_.prettyPrint()).either.unsafeRun
//  )

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
