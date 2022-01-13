package zio.config

import zio.config.ConfigDescriptor._
import zio.config.PropertyTree.{Leaf, Record, Sequence}
import zio.config.PropertyTreePath.Step.{Index, Key}
import zio.config.ReadError.{FormatError, ListErrors, ZipErrors}
import zio.test.Assertion._
import zio.test._

object ListsCornerCasesTest extends BaseSpec {

  val spec =
    suite("ListsCornerCasesTest")(
      test("read empty list") {
        case class Cfg(a: String, b: List[String])

        val cCfg = (string("a") zip list("b")(string)).to[Cfg]

        val res = read(
          cCfg from ConfigSource.fromPropertyTree(
            Record(Map("a" -> Leaf("sa"), "b" -> Sequence(Nil))),
            "tree"
          )
        )

        assertM(res)(equalTo(Cfg("sa", Nil)))
      },
      test("read nested lists") {
        case class Cfg(a: String, b: List[List[String]])

        val cCfg = (string("a") zip list("b")(list(string))).to[Cfg]

        val res =
          read(
            cCfg from ConfigSource.fromPropertyTree(
              Record(
                Map("a" -> Leaf("sa"), "b" -> Sequence(Sequence(Nil) :: Nil))
              ),
              "tree"
            )
          )

        assertM(res)(equalTo(Cfg("sa", Nil :: Nil)))
      },
      test("read absent optional lists") {
        case class Cfg(a: String, b: Option[List[String]])

        val cCfg =
          (string("a") zip list("b")(string).optional).to[Cfg]

        val res =
          read(
            cCfg from ConfigSource
              .fromPropertyTree(Record(Map("a" -> Leaf("sa"))), "tree")
          )

        assertM(res)(equalTo(Cfg("sa", None)))
      },
      test("read present optional empty lists") {
        case class Cfg(a: String, b: Option[List[String]])

        val cCfg =
          (string("a") zip list("b")(string).optional).to[Cfg]

        val res =
          read(
            cCfg from ConfigSource.fromPropertyTree(
              Record(Map("a" -> Leaf("sa"), "b" -> Sequence(Nil))),
              "tree"
            )
          )

        assertM(res)(equalTo(Cfg("sa", Some(Nil))))
      },
      test("use default value for absent list") {
        case class Cfg(a: String, b: List[String])

        val cCfg = (string("a") zip list("b")(string)
          .default("x" :: Nil)).to[Cfg]

        val res = read(
          cCfg from ConfigSource.fromPropertyTree(Record(Map("a" -> Leaf("sa"))), "tree")
        )

        assertM(res)(equalTo(Cfg("sa", "x" :: Nil)))
      },
      test("override default non-empty list with empty list") {
        case class Cfg(a: String, b: List[String])

        val cCfg = (string("a") zip list("b")(string)
          .default("x" :: Nil)).to[Cfg]

        val res = read(
          cCfg from ConfigSource.fromPropertyTree(
            Record(Map("a" -> Leaf("sa"), "b" -> Sequence(Nil))),
            "tree"
          )
        )

        assertM(res)(equalTo(Cfg("sa", Nil)))
      },
      test("distinguish list from scalar left") {
        case class Cfg(a: String, b: Either[List[String], String])

        val cCfg = (string("a") zip nested("b")(
          list(string).orElseEither(string)
        )).to[Cfg]

        val res =
          read(
            cCfg from ConfigSource.fromPropertyTree(
              Record(
                Map("a" -> Leaf("sa"), "b" -> Sequence(Leaf("v") :: Nil))
              ),
              "tree"
            )
          )

        assertM(res)(equalTo(Cfg("sa", Left("v" :: Nil))))
      },
      test("distinguish list from scalar right") {
        case class Cfg(a: String, b: Either[String, List[String]])

        val cCfg = (string("a") zip nested("b")(
          string.orElseEither(list(string))
        )).to[Cfg]

        val res =
          read(
            cCfg from ConfigSource.fromPropertyTree(
              Record(
                Map("a" -> Leaf("sa"), "b" -> Sequence(Leaf("v") :: Nil))
              ),
              "tree"
            )
          )

        assertM(res)(equalTo(Cfg("sa", Right("v" :: Nil))))
      },
      test("distinguish scalar from list left") {
        case class Cfg(a: String, b: Either[List[String], String])

        val cCfg = (string("a") zip nested("b")(
          list(string).orElseEither(string)
        )).to[Cfg]

        val res = read(
          cCfg from ConfigSource
            .fromPropertyTree(Record(Map("a" -> Leaf("sa"), "b" -> Leaf("v"))), "tree")
        )

        assertM(res)(equalTo(Cfg("sa", Left(List("v")))))
      },
      test("distinguish scalar from list right") {
        case class Cfg(a: String, b: Either[List[String], String])

        val cCfg = (string("a") zip nested("b")(
          list(string).orElseEither(string)
        )).to[Cfg]

        val res = read(
          cCfg from ConfigSource
            .fromPropertyTree(Record(Map("a" -> Leaf("sa"), "b" -> Leaf("v"))).leafNotASequence, "tree")
        )

        assertM(res)(equalTo(Cfg("sa", Right("v"))))
      },
      test("read list as scalar") {
        case class Cfg(a: String, b: String)

        val cCfg = (string("a") zip head("b")(string)).to[Cfg]

        val res = read(
          cCfg from ConfigSource.fromPropertyTree(
            Record(
              Map(
                "a" -> Leaf("sa"),
                "b" -> Sequence(Leaf("v1") :: Leaf("v2") :: Nil)
              )
            ),
            "tree"
          )
        )

        assertM(res)(equalTo(Cfg("sa", "v1")))
      },
      test("read single key objects in nested lists") {
        case class Cfg(a: String, b: List[List[String]])

        val cCfg = (string("a") zip list("b")(list(string("c")))).to[Cfg]

        val res = read(
          cCfg from ConfigSource.fromPropertyTree(
            Record(
              Map(
                "a" -> Leaf("sa"),
                "b" -> Sequence[String, String](
                  Sequence(Record(Map("c" -> Leaf("v1"))) :: Nil) ::
                    Sequence(Nil) ::
                    Sequence(
                      Record(Map("c" -> Leaf("v2"))) :: Record(
                        Map("c" -> Leaf("v3"))
                      ) :: Nil
                    ) ::
                    Nil
                )
              )
            ),
            "tree"
          )
        )

        assertM(res)(equalTo(Cfg("sa", List("v1") :: Nil :: List("v2", "v3") :: Nil)))
      },
      test("collect errors from list elements") {
        case class Cfg(a: String, b: List[String])

        val cCfg =
          (string("a") zip nested("b")(list(string))).to[Cfg]

        val res = read(
          cCfg from ConfigSource.fromPropertyTree(
            Record(
              Map(
                "a" -> Leaf("sa"),
                "b" -> Sequence(
                  Record[String, String](Map.empty) :: Sequence(Nil) :: Nil
                )
              )
            ),
            "tree"
          )
        )

        assertM(res.either)(isLeft(hasField[ReadError[String], Int]("size", _.size, equalTo(2))))
      },
      test("accumulates all errors") {
        case class Cfg(a: List[Boolean], b: List[Int])

        val cCfg = (nested("a")(list(boolean)) zip nested("b")(list(int))).to[Cfg]

        val res = read(
          cCfg from ConfigSource.fromPropertyTree(
            Record(
              Map(
                "a" -> Sequence(Leaf("true") :: Leaf("lorem ipsum") :: Nil),
                "b" -> Sequence(Leaf("one") :: Leaf("2") :: Nil)
              )
            ),
            "tree"
          )
        )

        val expected =
          ZipErrors(
            List(
              ListErrors(
                List(
                  FormatError(
                    List(Key("a"), Index(1)),
                    "Provided value is lorem ipsum, expecting the type boolean",
                    List("value of type boolean")
                  )
                )
              ),
              ListErrors(
                List(
                  FormatError(
                    List(Key("b"), Index(0)),
                    "Provided value is one, expecting the type int",
                    List("value of type int")
                  )
                )
              )
            )
          )

        assertM(res.either)(isLeft(equalTo(expected)))
      }
    )
}
