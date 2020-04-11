package zio.config.examples.typesafe

import zio.config.ConfigDescriptor
import zio.config._
import typesafe._
import ConfigDescriptor._
import com.typesafe.config.ConfigRenderOptions
import zio.config.typesafe.TypeSafeConfigSource

object MapExample extends App with EitherImpureOps {
  val hocon =
    s"""
       | zones: {
       |    syd  : [1, 2]
       |    melb : [1]
       |  }
       |
       |  l : []
       |
       |   l2: [1, 3, 3]
       |
       |  z : {
       |     v : a
       |  }
       |""".stripMargin

  val source = TypeSafeConfigSource.fromHoconString(hocon).loadOrThrow

  final case class sss(s: Map[String, List[Int]], l: List[Int], l2: List[Int], value: Map[String, String])

  val c1: ConfigDescriptor[String, String, Map[String, List[Int]]] =
    map("zones")(list(int))

  val c2 = list("l")(int)
  val c3 = list("l2")(int)

  val c4: ConfigDescriptor[String, String, Map[String, String]] =
    map("z")(string)

  val description =
    (c1 |@| c2 |@| c3 |@| c4)((a, b, c, d) => sss(a, b, c, d), sss.unapply)

  val result =
    read(description from source).loadOrThrow

  assert(result == sss(Map("melb" -> List(1), "syd" -> List(1, 2)), List(), List(1, 3, 3), Map("v" -> "a")))
  println(write(description, result).loadOrThrow.toHocon.render(ConfigRenderOptions.concise().setFormatted(true)))

  //  {
//    "l2" : [
//      "1",
//      "3",
//      "3"
//    ],
//    "z" : {
//      "v" : "a"
//    },
//    "zones" : {
//      "melb" : [
//        "1"
//       ],
//      "syd" : [
//        "1",
//        "2"
//        ]
//    }
//  }

  final case class Nested(s: Map[String, sss])

  val hocon2 =
    s"""
       |result : {
       | dynamic1 : {
       |       zones: {
       |         syd  : [1, 2]
       |         melb : [1]
       |      }
       |
       |       l : []
       |
       |       l2: [1, 3, 3]
       |
       |       z : {
       |         v : a
       |       }
       | }
       |
       | dynamic2 : {
       |      zones: {
       |        syd  : [1, 2]
       |        melb : [1]
       |      }
       |
       |      l : []
       |
       |     l2: [1, 3, 3]
       |
       |      z : {
       |         v : a
       |      }
       | }
       |}
       |""".stripMargin

  val result3 =
    read(nested("result")(mapStrict(description)) from TypeSafeConfigSource.fromHoconString(hocon2).loadOrThrow)

  println(result3)

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

  println(read(xx from TypeSafeConfigSource.fromHoconString(hocon3).loadOrThrow))

  val hocon4 =
    s"""
       |k : { dynamicMap : { y : z } }
       |""".stripMargin

  val xx2 = nested("k") { map(string("y")) }

  println(read(xx2 from TypeSafeConfigSource.fromHoconString(hocon4).loadOrThrow))

  println(generateDocs(map("s")(string) from ConfigSource.fromMap(Map.empty)))

}
