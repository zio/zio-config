package zio.config.typesafe

import zio.config.{BaseSpec, _}
import zio.test.Assertion._
import zio.test._

import ConfigDescriptor._
import TypesafeConfigMapSpecUtils._

object TypesafeConfigMapSpec extends BaseSpec {

  override def spec: Spec[Any, TestFailure[ReadError[String]], TestSuccess] = suite("Map Typesafe Integration")(
    test("read typesafe config") {
      val result =
        read(sssDescription from source)

      assertM(result)(
        equalTo(sss(Map("melb" -> List(1), "syd" -> List(1, 2)), List(), List(1, 3, 3), Map("v" -> "a")))
      )

    },
    test("read nested typesafe config map using map") {
      val source = TypesafeConfigSource.fromHoconString(hocon2)
      val result = read(
        nested("result")(map(sssDescription)).to[TypesafeConfigMapSpecUtils.Nested] from source
      )

      val expected =
        sss(Map("melb" -> List(1), "syd" -> List(1, 2)), List(), List(1, 3, 3), Map("v" -> "a"))

      assertM(result)(
        equalTo(TypesafeConfigMapSpecUtils.Nested(Map("dynamic1" -> expected, "dynamic2" -> expected)))
      )
    },
    test("map fetch the value of k when given map(string(k))") {
      val hocon3 =
        s"""
           |k : {
           |  s : {
           |     dynamicKey : { y : z }
           |  }
           |}
           |
           |y : z
           |""".stripMargin

      case class Cfg(map: Map[String, String], y: String)

      val desc = (nested("k")(map("s")(string("y"))) zip string("y")).to[Cfg]

      val result = read(desc from TypesafeConfigSource.fromHoconString(hocon3))

      assertM(result)(equalTo(Cfg(Map("dynamicKey" -> "z"), "z")))
    },
    test("map(string(y)) takes the value of y as a string and returns the map") {
      val hocon4 =
        s"""
           |k : {
           |  dynamicKey : {
           |     y : z
           |  }
           |  dynamicKey2 : {
           |     y : z2
           |     z : k
           |  }
           |
           |}
           |""".stripMargin

      val xx2 = nested("k")(map(string("y")))

      assertM(read(xx2 from TypesafeConfigSource.fromHoconString(hocon4)))(
        equalTo(Map("dynamicKey" -> "z", "dynamicKey2" -> "z2"))
      )
    }
  )
}

object TypesafeConfigMapSpecUtils {
  private val hocon =
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

  val source: ConfigSource = TypesafeConfigSource.fromHoconString(hocon)

  final case class sss(s: Map[String, List[Int]], l: List[Int], l2: List[Int], value: Map[String, String])

  val c1: ConfigDescriptor[Map[String, List[Int]]] =
    map("zones")(list(int))

  private val c2 = list("l")(int)
  private val c3 = list("l2")(int)

  val c4: ConfigDescriptor[Map[String, String]] =
    map("z")(string)

  val sssDescription: ConfigDescriptor[sss] =
    (c1 zip c2 zip c3 zip c4).to[sss]

  final case class Nested(s: Map[String, sss])

  val hocon2: String =
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

}
