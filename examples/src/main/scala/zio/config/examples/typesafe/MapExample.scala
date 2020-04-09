package zio.config.examples.typesafe

import zio.config.ConfigDescriptor
import zio.config._, ConfigDescriptor._
import zio.config.typesafe.TypeSafeConfigSource

object Hello extends App with EitherImpureOps {
  val hocon =
    s"""
       | zones: {
       |    syd  : [zone1, zone2]
       |    melb : [zone1]
       |    adl  : [zone1]
       |  }
       |
       |  z : {
       |     v : a
       |
       |  }
       |""".stripMargin

  val source =  TypeSafeConfigSource.fromHoconString(hocon).loadOrThrow
  final case class sss(s: Map[String, List[String]], value: Map[String, String])

  val c1: ConfigDescriptor[String, String, Map[String, List[String]]] =
    map("zones")(list(string))

  val c2: ConfigDescriptor[String, String, Map[String, String]] =
    nested("z") { mapStrict(string) }

  val result =
    (c1 |@| c2) ((a, b) => sss(a, b), sss.unapply )

  println(read( result from source))

}

