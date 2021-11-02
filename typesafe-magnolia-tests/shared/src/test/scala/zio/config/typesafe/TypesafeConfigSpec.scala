package zio.config.typesafe

import zio.config.PropertyTree.{Leaf, Record, Sequence}
import zio.config.ReadError
import zio.config.typesafe.TypesafeConfigTestSupport._
import zio.test.Assertion._
import zio.test._
import zio.config.PropertyTreePath

object TypesafeConfigSpec extends DefaultRunnableSpec {
  val spec: Spec[Any, TestFailure[ReadError[String]], TestSuccess] = suite("TypesafeConfig")(
    testM("Read empty list") {
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

      assertM(res.runTree(PropertyTreePath(Vector.empty)))(equalTo(expected))
    },
    testM("Read mixed list") {
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

      assertM(res.runTree(PropertyTreePath(Vector.empty)))(equalTo(expected))
    },
    testM("Read a complex hocon structure successfully") {
      assertM(readComplexSource)(equalTo(expectedResult))
    }
  )
}
