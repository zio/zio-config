package zio.config.typesafe

import zio.config.PropertyTree.{ Leaf, Record, Sequence }
import zio.config.typesafe.TypesafeConfigTestSupport._
import zio.test.Assertion._
import zio.test._

object TypesafeConfigSpec extends DefaultRunnableSpec {
  val spec = suite("TypesafeConfig")(
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

      val expected = Record(Map("a" -> Record(Map("b" -> Leaf("s"), "c" -> Sequence(Nil)))))

      assert(res.map(_.getConfigValue(List.empty)))(isRight(equalTo(expected)))
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

      val expected = Record(Map("list" -> Sequence(List(Leaf("a"), Record(Map("b" -> Leaf("c")))))))

      assert(res.map(_.getConfigValue(List.empty)))(isRight(equalTo(expected)))
    },
    test("Read a complex hocon structure successfully") {

      assert(readComplexSource)(equalTo(expectedResult))

    }
  )
}
