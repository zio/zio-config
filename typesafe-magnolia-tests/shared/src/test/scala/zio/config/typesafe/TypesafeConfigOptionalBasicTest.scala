package zio.config.typesafe

import zio.config.ReadError.Step.Key
import zio.config.ReadError.{Irrecoverable, MissingValue, ZipErrors}
import zio.config._
import zio.test.Assertion._
import zio.test._

import magnolia._

// A basic test before the set of TypesafeConfigOptionalTest
object TypesafeConfigOptionalBasicTest extends DefaultRunnableSpec with EitherSupport {
  val spec: ZSpec[Environment, Failure] = suite("Optional Spec")(
    test("Fails if any one of the required fields is missing in an optional product") {

      final case class RawConfig(tableDetails: List[RawTableConfig])

      final case class RawTableConfig(
        transformOptions: Option[TransformOptions]
      )

      final case class TransformOptions(
        header: String,
        separator: String,
        quoteAll: String
      )

      val string =
        s"""
           |      {
           |        "transformOptions" : {
           |            "quoteAll" : true
           |        }
           |      }
           |""".stripMargin

      val result: Either[ReadError[String], RawTableConfig] =
        read(descriptor[RawTableConfig] from TypesafeConfigSource.fromHoconString(string).loadOrThrow)

      val expected: Either[ReadError[String], RawTableConfig] =
        Left(
          Irrecoverable(
            List(
              ZipErrors(
                List(
                  ZipErrors(
                    List(
                      MissingValue(
                        List(Key("transformOptions"), Key("header")),
                        List("optional value", "value of type string"),
                        Set()
                      )
                    ),
                    Set()
                  ),
                  MissingValue(
                    List(Key("transformOptions"), Key("separator")),
                    List(
                      "optional value",
                      "value of " +
                        "type string"
                    ),
                    Set()
                  )
                ),
                Set()
              )
            ),
            Set()
          )
        )

      assert(result)(equalTo(expected))
    }
  )
}
