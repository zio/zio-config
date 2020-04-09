package zio.config.examples.typesafe

import zio.config.ConfigDescriptor
import zio.config._, ConfigDescriptor._
import zio.config.typesafe.TypeSafeConfigSource

object MapExample extends App with EitherImpureOps {
  val hocon =
    s"""
       | zones: {
       |    syd  : [1, 2]
       |    melb : [1]
       |    adl  : [1, 2, 3]
       |  }
       |
       |  l : [1, 2, 3]
       |
       |   l2: [1, 3m, 3]
       |
       |  z : {
       |     v : a
       |  }
       |""".stripMargin

  val source =  TypeSafeConfigSource.fromHoconString(hocon).loadOrThrow

  final case class sss(s: Map[String, List[Int]], l: List[Int], l2: List[Int], value: Map[String, String])

  val c1: ConfigDescriptor[String, String, Map[String, List[Int]]] =
    map("zones")(list(int))

  val c2 = list("l")(int)
  val c3 = list("l2")(int)

  val c4: ConfigDescriptor[String, String, Map[String, String]] =
    map("z")(string)

  val result =
    (c1 |@| c2 |@| c3 |@| c4) ((a, b, c, d) => sss(a, b, c, d ), sss.unapply )

  println(read( result from source))


  val wronHocon =
    s"""
       | zones: {
       |    syd  : [1, 2]
       |    melb : [1]
       |    adl  : [1, 2, 3]
       |  }
       |
       | l : [1, m, 3]
       |
       | l2: [1, 3m, 3]
       |
       |  z : {
       |     v : a
       |  }
       |""".stripMargin

  val wrongSource = TypeSafeConfigSource.fromHoconString(wronHocon).loadOrThrow

  println(read(result from wrongSource) )
}

