package zio.config.typesafe

import zio.config.magnolia.DeriveConfigDescriptor.descriptor
import zio.config.typesafe.OptionalSpecUtils._
import zio.config.typesafe.TypesafeConfigTestSupport._
import zio.config.{ BaseSpec, ConfigSource, _ }
import zio.test.Assertion._
import zio.test._
import ReadError.Step.Key

object OptionalSpec
    extends BaseSpec(
      suite("partial products fail instead of returning none")(
        test(
          "Presence of one optional value in an optional product cannot bypass failures"
        ) {
          val validConfig =
            s"""
               |      detail: {
               |       a : 10
               |       b : {
               |         c : 1
               |         e : {
               |           f : 1
               |         }
               |       }
               |       }
               |""".stripMargin

          val result =
            read(descriptor[TestCase1.CaseClass1] from getSource(validConfig))

          val summary =
            result.swap.map(t => (checkIfOnlyMissingValues(t), t.getListOfSteps)).swap

          assert(summary)(
            equalTo(
              Left(
                (
                  true,
                  List(
                    List(Key("detail"), Key("b"), Key("e"), Key("i")),
                    List(Key("detail"), Key("b"), Key("e"), Key("h"))
                  )
                )
              )
            )
          )
        },
        test(
          "Presence of another optional value in an optional product cannot bypass failures"
        ) {
          val validConfig =
            s"""
               |      detail: {
               |       a : 10
               |       b : {
               |         c : 1
               |         e : {
               |           g : 1
               |         }
               |       }
               |       }
               |""".stripMargin

          val result =
            read(descriptor[TestCase1.CaseClass1] from getSource(validConfig))

          val summary =
            result.swap.map(t => (checkIfOnlyMissingValues(t), t.getListOfSteps)).swap

          assert(summary)(
            equalTo(
              Left(
                (
                  true,
                  List(
                    List(Key("detail"), Key("b"), Key("e"), Key("i")),
                    List(Key("detail"), Key("b"), Key("e"), Key("h"))
                  )
                )
              )
            )
          )
        },
        test(
          "Presence of all optional values in an optional product cannot bypass failures"
        ) {
          val validConfig =
            s"""
               |      detail: {
               |       a : 10
               |       b : {
               |         c : 1
               |         e : {
               |           f : 1
               |           g : 1
               |         }
               |       }
               |       }
               |""".stripMargin

          val result =
            read(descriptor[TestCase1.CaseClass1] from getSource(validConfig))

          val summary =
            result.swap.map(t => (checkIfOnlyMissingValues(t), t.getListOfSteps)).swap

          assert(summary)(
            equalTo(
              Left(
                (
                  true,
                  List(
                    List(Key("detail"), Key("b"), Key("e"), Key("i")),
                    List(Key("detail"), Key("b"), Key("e"), Key("h"))
                  )
                )
              )
            )
          )
        },
        test(
          "Presence of one required value in an optional product cannot bypass failures"
        ) {
          val validConfig =
            s"""
               |      detail: {
               |       a : 10
               |       b : {
               |         c : 1
               |         e : {
               |           i : 1
               |         }
               |       }
               |       }
               |""".stripMargin

          val result =
            read(descriptor[TestCase1.CaseClass1] from getSource(validConfig))

          val summary =
            result.swap.map(t => (checkIfOnlyMissingValues(t), t.getListOfSteps)).swap

          assert(summary)(
            equalTo(
              Left(
                (
                  true,
                  List(
                    List(Key("detail"), Key("b"), Key("e"), Key("h"))
                  )
                )
              )
            )
          )
        },
        test(
          "Presence of another required value in an optional product cannot bypass failures"
        ) {
          val validConfig =
            s"""
               |      detail: {
               |       a : 10
               |       b : {
               |         c : 1
               |         e : {
               |           h : 1
               |         }
               |       }
               |       }
               |""".stripMargin

          val result =
            read(descriptor[TestCase1.CaseClass1] from getSource(validConfig))

          val summary =
            result.swap.map(t => (checkIfOnlyMissingValues(t), t.getListOfSteps)).swap

          assert(summary)(
            equalTo(
              Left(
                (
                  true,
                  List(
                    List(Key("detail"), Key("b"), Key("e"), Key("i"))
                  )
                )
              )
            )
          )
        },
        test(
          "Presence of one required value and one optional value cannot bypass failures"
        ) {
          val validConfig =
            s"""
               |      detail: {
               |       a : 10
               |       b : {
               |         c : 1
               |         e : {
               |           h : 1
               |           f : 1
               |         }
               |       }
               |       }
               |""".stripMargin

          val result =
            read(descriptor[TestCase1.CaseClass1] from getSource(validConfig))

          val summary =
            result.swap.map(t => (checkIfOnlyMissingValues(t), t.getListOfSteps)).swap

          assert(summary)(
            equalTo(
              Left(
                (
                  true,
                  List(
                    List(Key("detail"), Key("b"), Key("e"), Key("i"))
                  )
                )
              )
            )
          )
        },
        test(
          "Presence of another required value and another optional value cannot bypass failures"
        ) {
          val validConfig =
            s"""
               |      detail: {
               |       a : 10
               |       b : {
               |         c : 1
               |         e : {
               |           i : 1
               |           g : 1
               |         }
               |       }
               |       }
               |""".stripMargin

          val result =
            read(descriptor[TestCase1.CaseClass1] from getSource(validConfig))

          val summary =
            result.swap.map(t => (checkIfOnlyMissingValues(t), t.getListOfSteps)).swap

          assert(summary)(
            equalTo(
              Left(
                (
                  true,
                  List(
                    List(Key("detail"), Key("b"), Key("e"), Key("h"))
                  )
                )
              )
            )
          )
        },
        test(
          "Presence of all required values, and absence of all optional values returns some product"
        ) {
          val validConfig =
            s"""
               |      detail: {
               |       a : 10
               |       b : {
               |         c : 1
               |         e : {
               |           h : 1
               |           i : 1
               |         }
               |       }
               |       }
               |""".stripMargin

          val result =
            read(descriptor[TestCase1.CaseClass1] from getSource(validConfig))

          assert(result)(
            equalTo(
              Right(
                TestCase1.CaseClass1(
                  Some(
                    TestCase1.CaseClass2(
                      "10",
                      Some(TestCase1.CaseClass3("1", Some(TestCase1.CaseClass4(None, None, "1", "1"))))
                    )
                  )
                )
              )
            )
          )
        },
        test(
          "Presence of all required values, and presence of all optional values returns some product"
        ) {
          val validConfig =
            s"""
               |      detail: {
               |       a : 10
               |       b : {
               |         c : 1
               |         e : {
               |           h : 1
               |           i : 1
               |           f : 1
               |           g : 1
               |         }
               |       }
               |       }
               |""".stripMargin

          val result =
            read(descriptor[TestCase1.CaseClass1] from getSource(validConfig))

          assert(result)(
            equalTo(
              Right(
                TestCase1.CaseClass1(
                  Some(
                    TestCase1.CaseClass2(
                      "10",
                      Some(TestCase1.CaseClass3("1", Some(TestCase1.CaseClass4(Some("1"), Some("1"), "1", "1"))))
                    )
                  )
                )
              )
            )
          )
        },
        test(
          "Presence of all required values, and presence of any optional value returns some product"
        ) {
          val validConfig =
            s"""
               |      detail: {
               |       a : 10
               |       b : {
               |         c : 1
               |         e : {
               |           h : 1
               |           i : 1
               |           g : 1
               |         }
               |       }
               |       }
               |""".stripMargin

          val result =
            read(descriptor[TestCase1.CaseClass1] from getSource(validConfig))

          assert(result)(
            equalTo(
              Right(
                TestCase1.CaseClass1(
                  Some(
                    TestCase1.CaseClass2(
                      "10",
                      Some(TestCase1.CaseClass3("1", Some(TestCase1.CaseClass4(None, Some("1"), "1", "1"))))
                    )
                  )
                )
              )
            )
          )
        },
        test(
          "Presence of all required values, and presence of any other optional value returns some product"
        ) {
          val validConfig =
            s"""
               |      detail: {
               |       a : 10
               |       b : {
               |         c : 1
               |         e : {
               |           h : 1
               |           i : 1
               |           f : 1
               |         }
               |       }
               |       }
               |""".stripMargin

          val result =
            read(descriptor[TestCase1.CaseClass1] from getSource(validConfig))

          assert(result)(
            equalTo(
              Right(
                TestCase1.CaseClass1(
                  Some(
                    TestCase1.CaseClass2(
                      "10",
                      Some(TestCase1.CaseClass3("1", Some(TestCase1.CaseClass4(Some("1"), None, "1", "1"))))
                    )
                  )
                )
              )
            )
          )
        },
        test(
          "A null returns none in nested optional values"
        ) {
          val validConfig =
            s"""
               |      detail: {
               |       a : 10
               |       b : {
               |         c : 1
               |         e : null
               |       }
               |       }
               |""".stripMargin

          val result =
            read(descriptor[TestCase1.CaseClass1] from getSource(validConfig))

          assert(result)(
            equalTo(
              Right(
                TestCase1.CaseClass1(
                  Some(
                    TestCase1.CaseClass2(
                      "10",
                      Some(TestCase1.CaseClass3("1", None))
                    )
                  )
                )
              )
            )
          )
        },
        test(
          "Absence of all optional values and required values in an optional product returns none"
        ) {
          val validConfig =
            s"""
               |      detail: {
               |       a : 10
               |       b : {
               |         c : 1
               |         e : {
               |         }
               |       }
               |       }
               |""".stripMargin

          val result =
            read(descriptor[TestCase1.CaseClass1] from getSource(validConfig))

          assert(result)(
            equalTo(
              Right(TestCase1.CaseClass1(Some(TestCase1.CaseClass2("10", Some(TestCase1.CaseClass3("1", None))))))
            )
          )
        },
        test(
          "Absence of an optional product which itself has optional and required fields, within another optional product returns some of that product"
        ) {
          val validConfig =
            s"""
               |      detail: {
               |       a : 10
               |       b : {
               |         c : 1
               |         e : null
               |       }
               |       }
               |""".stripMargin

          val result =
            read(descriptor[TestCase1.CaseClass1] from getSource(validConfig))

          assert(result)(
            equalTo(
              Right(TestCase1.CaseClass1(Some(TestCase1.CaseClass2("10", Some(TestCase1.CaseClass3("1", None))))))
            )
          )
        },
        test(
          "Absence of an optional product and a required field within another optional product returns none"
        ) {
          val validConfig =
            s"""
               |      detail: {
               |         a : 10
               |         b : {}
               |       }
               |""".stripMargin

          val result =
            read(descriptor[TestCase1.CaseClass1] from getSource(validConfig))

          assert(result)(
            equalTo(
              Right(TestCase1.CaseClass1(Some(TestCase1.CaseClass2("10", None))))
            )
          )
        },
        test(
          "Absence of a required field and presence of an optional product within another optional product returns failure"
        ) {
          val validConfig =
            s"""
               |      detail: {
               |         a : 10
               |         b : {
               |           e : {
               |             h : 1
               |             i : 1
               |           }
               |         }
               |       }
               |""".stripMargin

          val result =
            read(descriptor[TestCase1.CaseClass1] from getSource(validConfig))

          val summary =
            result.swap.map(t => (checkIfOnlyMissingValues(t), t.getListOfSteps)).swap

          assert(summary)(
            equalTo(
              Left(
                (
                  true,
                  List(
                    List(Key("detail"), Key("b"), Key("c"))
                  )
                )
              )
            )
          )
        }
      )
    )

object OptionalSpecUtils {
  object TestCase1 {
    case class CaseClass1(detail: Option[CaseClass2])
    case class CaseClass2(a: String, b: Option[CaseClass3])
    case class CaseClass3(c: String, e: Option[CaseClass4])
    case class CaseClass4(f: Option[String], g: Option[String], i: String, h: String)
  }

  object TestCase2 {
    final case class CaseClass1(a: Option[CaseClass2])
    final case class CaseClass2(a: String, b: Either[Int, CaseClass3])
    final case class CaseClass3(a: String, b: String, c: Option[Int], d: Option[Int], e: String, f: Option[Int])
  }

  def checkIfOnlyMissingValues[K](error: ReadError[K]): Boolean =
    error.fold(false) {
      case ReadError.MissingValue(_, _) => true
    }(_ && _, true)

  def getSource(str: String): ConfigSource =
    TypesafeConfigSource.fromHoconString(str).loadOrThrow
}
