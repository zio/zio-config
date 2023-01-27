package zio.config.typesafe

import zio.config.PropertyTree.{Leaf, Record, Sequence}
import zio.config.typesafe.TypesafeConfigTestSupport._
import zio.config.{PropertyTreePath, ReadError}
import zio.test.Assertion._
import zio.test.{ZIOSpecDefault, _}

object TypesafeConfigSpec extends ZIOSpecDefault {
  val spec: Spec[Any, Config.Error] = suite("TypesafeConfig")(
    test("Read empty list") {
      val res =
        TypesafeConfigSource.fromHoconString(
          """
            |a {
            |  b = "s"
            |  c = []
            |}
            |""".stripMargin
        )

      val expected = Record(Map("a" -> Record(Map("b" -> Leaf("s", false), "c" -> Sequence(Nil)))))

      assertZIO(res.runTree(PropertyTreePath(Vector.empty)))(equalTo(expected))
    },
    test("Read mixed list") {
      val res =
        TypesafeConfigSource.fromHoconString(
          """
            |list = [
            |  "a",
            |  {b = "c"}
            |]
            |""".stripMargin
        )

      val expected = Record(Map("list" -> Sequence(List(Leaf("a", false), Record(Map("b" -> Leaf("c", false)))))))

      assertZIO(res.runTree(PropertyTreePath(Vector.empty)))(equalTo(expected))
    },
    test("Read a complex hocon structure successfully") {
      assertZIO(readComplexSource)(equalTo(expectedResult))
    }
  )
}
