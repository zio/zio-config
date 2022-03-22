package zio.config.examples.magnolia

import zio.config._
import zio.config.examples.typesafe.EitherImpureOps
import zio.config.magnolia.descriptor
import zio.config.typesafe.TypesafeConfigSource

import examples._

sealed trait X

object X {
  case object A extends X
  case object B extends X

  final case class C(value: String, g: G)             extends X
  final case class D(value: Y)                        extends X
  final case class E(a: String, b: Int)               extends X
  final case class F(a: String, b: Option[Int], c: Y) extends X
}

sealed trait Y
case class Z(a: String) extends Y

case class G(l: String)

case class Cfg(fieldName: X)
case class CfgCfg(cfg: Cfg, n: Int, c: String)

object Cfg extends App with EitherImpureOps {

  import X._

  val s1: String =
    """
      |cfg {
      | fieldName {
      |   C {
      |     value = b
      |     g {
      |       l = hi
      |     }
      |    }
      |  }
      |}
      |n = 1
      |c = l
      |
      |""".stripMargin

  assert(
    read(descriptor[CfgCfg] from TypesafeConfigSource.fromHoconString(s1)) equalM
      CfgCfg(Cfg(C("b", G("hi"))), 1, "l")
  )

  val s2: String =
    """
      |fieldName = A
      |""".stripMargin

  assert(read(descriptor[Cfg] from TypesafeConfigSource.fromHoconString(s2)) equalM Cfg(A))

  val s3: String =
    """
      |fieldName = B
      |""".stripMargin

  assert(read(descriptor[Cfg] from TypesafeConfigSource.fromHoconString(s3)) equalM Cfg(B))

  val s4: String =
    """
      |fieldName {
      |  D {
      |   value {
      |       Z {
      |         a = 1
      |       }
      |     }
      |  }
      |}
      |""".stripMargin

  assert(read(descriptor[Cfg] from TypesafeConfigSource.fromHoconString(s4)) equalM Cfg(D(Z("1"))))

  val s5: String =
    """
      |fieldName {
      |    E {
      |      a = 1
      |      b = 2
      |    }
      |}
      |""".stripMargin

  assert(read(descriptor[Cfg] from TypesafeConfigSource.fromHoconString(s5)) equalM Cfg(E("1", 2)))

  val s6: String =
    """
      |fieldName {
      |    F {
      |      a = 1
      |      c {
      |          Z {
      |            a = 2
      |          }
      |      }
      |    }
      |}
      |""".stripMargin

  assert(
    read(descriptor[Cfg] from TypesafeConfigSource.fromHoconString(s6)) equalM
      Cfg(F("1", None, Z("2")))
  )

  val s7: String =
    """
      |fieldName {
      |    F {
      |      a = 1
      |      b = 2
      |      c {
      |          Z {
      |            a = 2
      |          }
      |      }
      |    }
      |}
      |""".stripMargin

  assert(
    read(descriptor[Cfg] from TypesafeConfigSource.fromHoconString(s7)) equalM
      Cfg(F("1", Some(2), Z("2")))
  )

}
