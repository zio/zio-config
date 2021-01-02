package zio.config.typesafe

import zio.test.Assertion._
import zio.test._
import zio.config._, zio.config.magnolia.descriptor

object TypesafeRecursiveConfigTest extends DefaultRunnableSpec with EitherSupport {
  val spec = suite("TypesafeConfigRecursiveAutomatic")(
    test("Read recursive typesafe config with optional") {
      case class SimpleRec(id: Int, s: Option[SimpleRec])

      val res =
        s"""
           |{
           |  id : 1
           |  s : {
           |    id : 2
           |  }
           |}
           |""".stripMargin

      val result = read(descriptor[SimpleRec] from TypesafeConfigSource.fromHoconString(res).loadOrThrow)

      assert(result)(isRight(equalTo(SimpleRec(1, Some(SimpleRec(2, None))))))
    },
    test("Read recursive typesafe config with list") {
      case class SimpleRec(id: Int, s: List[SimpleRec])

      val res =
        s"""
           |{
           |  id : 1
           |  s : [{
           |    id : 2,
           |    s : []
           |  }]
           |}
           |""".stripMargin

      val result = read(descriptor[SimpleRec] from TypesafeConfigSource.fromHoconString(res).loadOrThrow)

      assert(result)(isRight(equalTo(SimpleRec(1, List(SimpleRec(2, Nil))))))
    },
    test("Read recursive typesafe config with either") {
      case class SimpleRec(id: Int, s: Either[SimpleRec, Int])

      val res =
        s"""
           |{
           |  id : 1
           |  s : {
           |    id : 2,
           |    s : 3
           |  }
           |}
           |""".stripMargin

      val result = read(descriptor[SimpleRec] from TypesafeConfigSource.fromHoconString(res).loadOrThrow)

      assert(result)(isRight(equalTo(SimpleRec(1, Left(SimpleRec(2, Right(3)))))))
    },
    test("Read recursive typesafe config with map") {
      case class SimpleRec(id: Int, s: Map[String, SimpleRec])

      val res =
        s"""
           |{
           |  id : 1
           |  s : {
           |    11 : {
           |      id : 22,
           |      s : {}
           |    },
           |    12 : {
           |      id : 33,
           |      s : {
           |        13 : {
           |          id : 44,
           |          s : {}
           |        }
           |      }
           |    }
           |  }
           |}
           |""".stripMargin

      val result = read(descriptor[SimpleRec] from TypesafeConfigSource.fromHoconString(res).loadOrThrow)

      assert(result)(
        isRight(
          equalTo(
            SimpleRec(
              1,
              Map("12" -> SimpleRec(33, Map("13" -> SimpleRec(44, Map.empty))), "11" -> SimpleRec(22, Map.empty))
            )
          )
        )
      )
    }
  )
}
