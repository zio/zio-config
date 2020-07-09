package zio.config.typesafe

import zio.config.ReadError.Step.Key
import zio.config.{ read, BaseSpec, ReadError }
import zio.test._
import zio.test.Assertion._
import zio.config.magnolia.DeriveConfigDescriptor.descriptor
import zio.config.typesafe.OptionalSpecUtils._
import ReadError._
import zio.config.foldReadError

object TypesafeConfigOptionalTest
    extends BaseSpec(
      suite("partial products fail instead of returning none")(
        test(
          "Presence of one optional value in an optional product with required fields returns failures"
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
            read(descriptor[TestCase1.CaseClass1] from OptionalSpecUtils.getSource(validConfig))

          val summary =
            result.swap.map(t => (OptionalSpecUtils.checkIfOnlyMissingValues(t), getListOfMissingValueSteps(t))).swap

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
          "Presence of another optional value in an optional product with required fields returns failures"
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
            result.swap.map(t => (checkIfOnlyMissingValues(t), getListOfMissingValueSteps(t))).swap

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
          "Presence of all optional values in an optional product with required fields  returns failures"
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
            result.swap.map(t => (checkIfOnlyMissingValues(t), getListOfMissingValueSteps(t))).swap

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
          "Presence of one required value in an optional product with multiple required fields returns failures"
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
            result.swap.map(t => (checkIfOnlyMissingValues(t), getListOfMissingValueSteps(t))).swap

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
          "Presence of another required value in an optional product with multiple required fields returns failures"
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
            result.swap.map(t => (checkIfOnlyMissingValues(t), getListOfMissingValueSteps(t))).swap

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
          "Presence of one required value and one optional value in an optional product with multiple required and optional fields returns failures"
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
            result.swap.map(t => (checkIfOnlyMissingValues(t), getListOfMissingValueSteps(t))).swap

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
          "Presence of another required value and another optional value within an optional product with multiple required and optional fields returns failures"
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
            result.swap.map(t => (checkIfOnlyMissingValues(t), getListOfMissingValueSteps(t))).swap

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
            result.swap.map(t => (checkIfOnlyMissingValues(t), getListOfMissingValueSteps(t))).swap

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
        },
        test(
          "An either[a, product] in an optional product returns failure if product is partially applied"
        ) {
          val validConfig =
            s"""
               |      a: {
               |         a : 10
               |         b : {
               |           a : 1
               |           b : 1
               |           c : 2
               |           d : 1
               |           f : 1
               |         }
               |       }
               |""".stripMargin

          val result =
            read(descriptor[TestCase2.CaseClass1] from getSource(validConfig))

          val summary =
            result.swap.map(fetchMissingValueAndFormatErrors).swap

          assert(summary)(
            equalTo(
              Left(
                List(
                  ReadError
                    .MissingValue(List(Key("a"), Key("b"), Key("e")), List("optional value", "value of type string")),
                  ReadError.FormatError(
                    List(Key("a"), Key("b")),
                    "Provided value is of type Record, expecting the type Leaf",
                    List("optional value", "value of type int")
                  )
                )
              )
            )
          )
        },
        test(
          "An either[a, product] in an optional product returns format failures on the left, and reports missing value on the right"
        ) {
          val validConfig =
            s"""
               |      a: {
               |         a : 10
               |         b : "jj"
               |       }
               |""".stripMargin

          val result =
            read(descriptor[TestCase2.CaseClass1] from getSource(validConfig))

          val summary =
            result.swap.map(fetchMissingValueAndFormatErrors).swap

          assert(summary)(
            equalTo(
              Left(
                List(
                  MissingValue(List(Key("a"), Key("b"), Key("b")), List("optional value", "value of type string")),
                  MissingValue(List(Key("a"), Key("b"), Key("e")), List("optional value", "value of type string")),
                  MissingValue(List(Key("a"), Key("b"), Key("a")), List("optional value", "value of type string")),
                  FormatError(List(Key("a"), Key("b")), "Provided value is jj, expecting the type int")
                )
              )
            )
          )
        },
        test(
          "An either[a, product] in an optional product failures on the right, and reports missing value on the left and right"
        ) {
          val validConfig =
            s"""
               |      a: {
               |         a : 10
               |         b : {
               |           c : nonint
               |         }
               |       }
               |""".stripMargin

          val result =
            read(descriptor[TestCase2.CaseClass1] from getSource(validConfig))

          val summary =
            result.swap.map(fetchMissingValueAndFormatErrors).swap

          assert(summary)(
            equalTo(
              Left(
                List(
                  MissingValue(List(Key("a"), Key("b"), Key("b")), List("optional value", "value of type string")),
                  FormatError(List(Key("a"), Key("b"), Key("c")), "Provided value is nonint, expecting the type int"),
                  MissingValue(List(Key("a"), Key("b"), Key("e")), List("optional value", "value of type string")),
                  MissingValue(List(Key("a"), Key("b"), Key("a")), List("optional value", "value of type string")),
                  FormatError(
                    List(Key("a"), Key("b")),
                    "Provided value is of type Record, expecting the type Leaf",
                    List("optional value", "value of type int")
                  )
                )
              )
            )
          )
        },
        test(
          "An optional either[a, product] returns failures if product is partially applied"
        ) {
          val validConfig =
            s"""
               |      a: {
               |         a : 10
               |         b : {
               |           a : 1
               |           b : 1
               |         }
               |       }
               |""".stripMargin

          val result =
            read(descriptor[TestCase3.CaseClass1] from getSource(validConfig))

          val summary =
            result.swap.map(fetchMissingValueAndFormatErrors).swap

          assert(summary)(
            equalTo(
              Left(
                List(
                  MissingValue(List(Key("a"), Key("b"), Key("e")), List("optional value", "value of type string")),
                  FormatError(
                    List(Key("a"), Key("b")),
                    "Provided value is of type Record, expecting the type Leaf",
                    List("optional value", "value of type int")
                  )
                )
              )
            )
          )
        },
        test(
          "An optional either[a, product] returns none if the parent key is missing"
        ) {
          val validConfig =
            s"""
               |      a: {
               |         a : 10
               |       }
               |""".stripMargin

          val result =
            read(descriptor[TestCase3.CaseClass1] from getSource(validConfig))

          val summary =
            result.swap.map(fetchMissingValueAndFormatErrors).swap

          assert(summary)(
            equalTo(
              Right(TestCase3.CaseClass1(TestCase3.CaseClass2("10", None)))
            )
          )
        },
        test(
          "An optional either[a, product] returns none if the parent key is null"
        ) {
          val validConfig =
            s"""
               |      a: {
               |         a : 10
               |         b : null
               |       }
               |""".stripMargin

          val result =
            read(descriptor[TestCase3.CaseClass1] from getSource(validConfig))

          val summary =
            result.swap.map(fetchMissingValueAndFormatErrors).swap

          assert(summary)(
            equalTo(
              Right(TestCase3.CaseClass1(TestCase3.CaseClass2("10", None)))
            )
          )
        },
        test(
          "An optional product with multiple either[product1, product2] returns failures if any either has partial configs"
        ) {
          val validConfig =
            s"""
               |      a: {
               |         a : { a1 : 10 }
               |       }
               |""".stripMargin

          val result =
            read(descriptor[TestCase4.CaseClass1] from getSource(validConfig))

          val summary =
            result.swap.map(fetchMissingValueAndFormatErrors).swap

          assert(summary)(
            equalTo(
              Left(
                List(
                  MissingValue(List(Key("a"), Key("c"), Key("b2")), List("optional value", "value of type string")),
                  MissingValue(List(Key("a"), Key("c"), Key("a2")), List("optional value", "value of type string")),
                  MissingValue(List(Key("a"), Key("c"), Key("b1")), List("optional value", "value of type string")),
                  MissingValue(List(Key("a"), Key("c"), Key("a1")), List("optional value", "value of type string")),
                  MissingValue(List(Key("a"), Key("a"), Key("b2")), List("optional value", "value of type string")),
                  MissingValue(List(Key("a"), Key("a"), Key("a2")), List("optional value", "value of type string")),
                  MissingValue(List(Key("a"), Key("a"), Key("b1")), List("optional value", "value of type string"))
                )
              )
            )
          )
        },
        test(
          "An optional product with 2 required either[product1, product2] and 1 optional either[product1, product2] returns failures if optional value is present"
        ) {
          val validConfig =
            s"""
               |      a: {
               |         b : { 
               |           a1 : 10
               |           b1 : 10
               |         }
               |       }
               |""".stripMargin

          val result =
            read(descriptor[TestCase4.CaseClass1] from getSource(validConfig))

          val summary =
            result.swap.map(fetchMissingValueAndFormatErrors).swap

          assert(summary)(
            equalTo(
              Left(
                List(
                  MissingValue(List(Key("a"), Key("c"), Key("b2")), List("optional value", "value of type string")),
                  MissingValue(List(Key("a"), Key("c"), Key("a2")), List("optional value", "value of type string")),
                  MissingValue(List(Key("a"), Key("c"), Key("b1")), List("optional value", "value of type string")),
                  MissingValue(List(Key("a"), Key("c"), Key("a1")), List("optional value", "value of type string")),
                  MissingValue(List(Key("a"), Key("a"), Key("b2")), List("optional value", "value of type string")),
                  MissingValue(List(Key("a"), Key("a"), Key("a2")), List("optional value", "value of type string")),
                  MissingValue(List(Key("a"), Key("a"), Key("b1")), List("optional value", "value of type string")),
                  MissingValue(List(Key("a"), Key("a"), Key("a1")), List("optional value", "value of type string"))
                )
              )
            )
          )
        },
        test(
          "An optional product with 2 required either[product1, product2] and 1 optional either[product1, product2] returns failures if optional value is present partially"
        ) {
          val validConfig =
            s"""
               |      a: {
               |         b : { 
               |           a1 : 10
               |         }
               |       }
               |""".stripMargin

          val result =
            read(descriptor[TestCase4.CaseClass1] from getSource(validConfig))

          val summary =
            result.swap.map(fetchMissingValueAndFormatErrors).swap

          assert(summary)(
            equalTo(
              Left(
                List(
                  MissingValue(
                    List(Key("a"), Key("b"), Key("b2")),
                    List("optional value", "optional value", "value of type string")
                  ),
                  MissingValue(
                    List(Key("a"), Key("b"), Key("a2")),
                    List("optional value", "optional value", "value of type string")
                  ),
                  MissingValue(
                    List(Key("a"), Key("b"), Key("b1")),
                    List("optional value", "optional value", "value of type string")
                  ),
                  MissingValue(List(Key("a"), Key("c"), Key("b2")), List("optional value", "value of type string")),
                  MissingValue(List(Key("a"), Key("c"), Key("a2")), List("optional value", "value of type string")),
                  MissingValue(List(Key("a"), Key("c"), Key("b1")), List("optional value", "value of type string")),
                  MissingValue(List(Key("a"), Key("c"), Key("a1")), List("optional value", "value of type string")),
                  MissingValue(List(Key("a"), Key("a"), Key("b2")), List("optional value", "value of type string")),
                  MissingValue(List(Key("a"), Key("a"), Key("a2")), List("optional value", "value of type string")),
                  MissingValue(List(Key("a"), Key("a"), Key("b1")), List("optional value", "value of type string")),
                  MissingValue(List(Key("a"), Key("a"), Key("a1")), List("optional value", "value of type string"))
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

  object TestCase3 {
    final case class CaseClass1(a: CaseClass2)
    final case class CaseClass2(a: String, b: Option[Either[Int, CaseClass3]])
    final case class CaseClass3(a: String, b: String, c: Option[Int], d: Option[Int], e: String, f: Option[Int])
  }

  object TestCase4 {
    final case class CaseClass1(a: Option[CaseClass2])
    final case class CaseClass2(
      a: Either[CaseClass3, CaseClass4],
      b: Option[Either[CaseClass3, CaseClass4]],
      c: Either[CaseClass3, CaseClass4]
    )
    final case class CaseClass3(a1: String, b1: String)
    final case class CaseClass4(a2: String, b2: String)
  }

  final def getListOfMissingValueSteps(error: ReadError[String]): List[List[Step[String]]] =
    foldReadError(error)(List.empty[List[Step[String]]]) {
      case ReadError.MissingValue(steps, _, _) => List(steps)
    }(_ ++ _, List.empty[List[Step[String]]])

  def checkIfOnlyMissingValues(error: ReadError[String]): Boolean =
    foldReadError(error)(alternative = false) {
      case ReadError.MissingValue(_, _, _) => true
    }(_ && _, true)

  def fetchMissingValueAndFormatErrors(error: ReadError[String]): List[ReadError[String]] =
    foldReadError(error)(alternative = List.empty[ReadError[String]]) {
      case e @ ReadError.MissingValue(_, _, _)   => List(e)
      case e @ ReadError.FormatError(_, _, _, _) => List(e)
    }(_ ++ _, List.empty[ReadError[String]])

  def getSource(str: String): zio.config.ConfigSource =
    TypesafeConfigSource.fromHoconString(str).fold(_ => throw new Exception("failed"), identity)
}
