package zio.config.examples.typesafe

import java.util.Properties

import zio.config.ConfigDescriptor
import zio.config._
//import typesafe._
import ConfigDescriptor._
//import com.typesafe.config.ConfigRenderOptions
//import zio.config.typesafe.TypesafeConfigSource

object MapExample extends App with EitherImpureOps {
/*  val hocon =
    s"""
       | zones: {
       |    syd  : [1, 2]
       |    melb : [1]
       |  }
       |
       |  l : []
       |
       |  l2: [1, 3, 3]
       |
       |  z : {
       |     v : a
       |  }
       |""".stripMargin

  val source = TypesafeConfigSource.fromHoconString(hocon).loadOrThrow

  final case class Cfg(zones: Map[String, List[Int]], l: List[Int], l2: List[Int], value: Map[String, String])

  val c1: ConfigDescriptor[String, String, Map[String, List[Int]]] =
    map("zones")(list(int))

  val c2 = list("l")(int)
  val c3 = list("l2")(int)

  val c4: ConfigDescriptor[String, String, Map[String, String]] =
    map("z")(string)

  val description =
    (c1 |@| c2 |@| c3 |@| c4)((a, b, c, d) => Cfg(a, b, c, d), Cfg.unapply)

  val result =
    read(description from source).loadOrThrow

  assert(result == Cfg(Map("melb" -> List(1), "syd" -> List(1, 2)), List(), List(1, 3, 3), Map("v" -> "a")))

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

  final case class Nested(s: Map[String, Cfg])

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
    read(nested("result")(mapStrict(description)) from TypesafeConfigSource.fromHoconString(hocon2).loadOrThrow)

  assert(
    result3 ==
      Right(
        Map(
          "dynamic2" -> Cfg(Map("melb" -> List(1), "syd" -> List(1, 2)), List(), List(1, 3, 3), Map("v" -> "a")),
          "dynamic1" -> Cfg(Map("melb" -> List(1), "syd" -> List(1, 2)), List(), List(1, 3, 3), Map("v" -> "a"))
        )
      )
  )

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

  assert(read(xx from TypesafeConfigSource.fromHoconString(hocon3).loadOrThrow) == Right(Map("dynamicMap" -> "z")))

  val hocon4 =
    s"""
       |k : { dynamicMap : { y : z } }
       |""".stripMargin

  val xx2 = nested("k") { map(string("y")) }

  assert(read(xx2 from TypesafeConfigSource.fromHoconString(hocon4).loadOrThrow) == Right(Map("dynamicMap" -> "z")))

  // Reporting of map values (More to come in this space: Fixme: https://github.com/zio/zio-config/issues/287)
  //import ConfigDocs._
*/
  val source2 =
    ConfigSource.fromMap(Map("key" -> "1"), keyDelimiter = Some('.'), valueDelimter = Some(','))

  val source1 =
    ConfigSource.fromProperties(new Properties())

  println(read(listStrict("key")(string) from source2))
  //println(read(list("key")((int from source2).orElseEither(string from source1))))

  //println(generateReport(map("key")(string from source1.orElse(source2)), Map("d1" -> "value1", "d2" -> "value2")))

//  assert(
//    generateReport(map("key")(string) from ConfigSource.fromMap(Map()), Map("d1" -> "value1", "d2" -> "value2")) ==
//      Right(
//        ConfigDocs.Nested(
//          "key",
//          DynamicMap(
//            Map(
//              "d2" -> Leaf((Set(ConfigSource.Name("constant"))), List("value of type string"), Some("value2")),
//              "d1" -> Leaf((Set(ConfigSource.Name("constant"))), List("value of type string"), Some("value1"))
//            )
//          )
//        )
//      )
//  )

}
