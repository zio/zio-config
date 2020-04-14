package zio.config.examples.magnolia

import zio.config._
import zio.config.examples.typesafe.EitherImpureOps
import zio.config.magnolia.DeriveConfigDescriptor.descriptor
import zio.config.typesafe.TypesafeConfigSource

sealed trait X
case object A                                 extends X
case object B                                 extends X
case class C(value: String, g: G)             extends X
case class D(value: Y)                        extends X
case class E(a: String, b: Int)               extends X
case class F(a: String, b: Option[Int], c: Y) extends X

sealed trait Y
case class Z(a: String) extends Y

case class G(l: String)

case class Cfg(fieldName: X)
case class CfgCfg(cfg: Cfg, n: Int, c: String)

case class K(a: String)

// Fix me: Add to tests (not possible currently coz modules are independent of each other)
object Cfg extends App with EitherImpureOps {

  val s1 =
    """
      |cfg{
      | fieldName {
      |  x {
      |   c {
      |     value = b
      |     g {
      |       l = hi
      |     }
      |    }
      |  }
      | }
      |}
      |n = 1
      |c = l
      |
      |""".stripMargin

  assert(
    read(descriptor[CfgCfg] from TypesafeConfigSource.fromHoconString(s1).loadOrThrow) == Right(
      CfgCfg(Cfg(C("b", G("hi"))), 1, "l")
    )
  )

  val s2 =
    """
      |fieldName {
      |  x = a
      |}
      |""".stripMargin

  assert(
    read(descriptor[Cfg] from TypesafeConfigSource.fromHoconString(s2).loadOrThrow) == Right(Cfg(A))
  )

  val s3 =
    """
      |fieldName {
      |  x = b
      |}
      |""".stripMargin

  assert(
    read(descriptor[Cfg] from TypesafeConfigSource.fromHoconString(s3).loadOrThrow) == Right(Cfg(B))
  )

  val s4 =
    """
      |fieldName {
      | x {
      |  d {
      |   value {
      |     y {
      |       z {
      |         a = 1
      |       }
      |     }
      |    }
      |  }
      | }
      |}
      |""".stripMargin

  assert(
    read(descriptor[Cfg] from TypesafeConfigSource.fromHoconString(s4).loadOrThrow) == Right(Cfg(D(Z("1"))))
  )

  val s5 =
    """
      |fieldName {
      |  x {
      |    e {
      |      a = 1
      |      b = 2
      |    }
      |  
      |  }
      |}
      |""".stripMargin

  assert(
    read(descriptor[Cfg] from TypesafeConfigSource.fromHoconString(s5).loadOrThrow) == Right(Cfg(E("1", 2)))
  )

  val s6 =
    """
      |fieldName {
      |  x {
      |    f {
      |      a = 1
      |      c {
      |        y {
      |          z {
      |            a = 2
      |          }
      |        }
      |      }
      |    }
      |  
      |  }
      |}
      |""".stripMargin

  assert(
    read(descriptor[Cfg] from TypesafeConfigSource.fromHoconString(s6).loadOrThrow) == Right(
      Cfg(F("1", None, Z("2")))
    )
  )

  val s7 =
    """
      |fieldName {
      |  x {
      |    f {
      |      a = 1
      |      b = 2
      |      c {
      |        y {
      |          z {
      |            a = 2
      |          }
      |        }
      |      }
      |    }
      |  
      |  }
      |}
      |""".stripMargin

  assert(
    read(descriptor[Cfg] from TypesafeConfigSource.fromHoconString(s7).loadOrThrow) == Right(
      Cfg(F("1", Some(2), Z("2")))
    )
  )

}
