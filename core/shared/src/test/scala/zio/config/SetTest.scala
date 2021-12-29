package zio.config

import zio.config.ConfigDescriptor._
import zio.config.PropertyTree.{Leaf, Record, Sequence}
import zio.config.PropertyTreePath.Step.{Index, Key}
import zio.config.ReadError.{ConversionError, FormatError, ListErrors, MissingValue, ZipErrors}
import zio.test.Assertion._
import zio.test._

object SetTest extends BaseSpec {

  val spec: ZSpec[Environment, Failure] =
    suite("SetsTest")(
      testM("read empty set") {
        case class Cfg(a: String, b: Set[String])

        val cCfg = (string("a") zip set("b")(string)).to[Cfg]

        val res = read(
          cCfg from ConfigSource.fromPropertyTree(
            Record(Map("a" -> Leaf("sa"), "b" -> Sequence(Nil))),
            "tree"
          )
        )

        assertM(res)(equalTo(Cfg("sa", Set.empty)))
      },
      testM("read nested sets") {
        case class Cfg(a: String, b: Set[Set[String]])

        val cCfg = (string("a") zip set("b")(set(string))).to[Cfg]

        val res =
          read(
            cCfg from ConfigSource.fromPropertyTree(
              Record(Map("a" -> Leaf("sa"), "b" -> Sequence(Sequence(Nil) :: Nil))),
              "tree"
            )
          )

        assertM(res)(equalTo(Cfg("sa", Set(Set.empty))))
      },
      testM("read absent optional sets") {
        case class Cfg(a: String, b: Option[Set[String]])

        val cCfg = (string("a") zip set("b")(string).optional).to[Cfg]

        val res =
          read(
            cCfg from ConfigSource.fromPropertyTree(
              Record(Map("a" -> Leaf("sa"))),
              "tree"
            )
          )

        assertM(res)(equalTo(Cfg("sa", None)))
      },
      testM("read present optional empty sets") {
        case class Cfg(a: String, b: Option[Set[String]])

        val cCfg = (string("a") zip set("b")(string).optional).to[Cfg]

        val res =
          read(
            cCfg from ConfigSource.fromPropertyTree(
              Record(Map("a" -> Leaf("sa"), "b" -> Sequence(Nil))),
              "tree"
            )
          )

        assertM(res)(equalTo(Cfg("sa", Some(Set.empty))))
      },
      testM("use default value for absent set") {
        case class Cfg(a: String, b: Set[String])

        val cCfg = (string("a") zip set("b")(string).default(Set("x"))).to[Cfg]

        val res = read(
          cCfg from ConfigSource.fromPropertyTree(
            Record(Map("a" -> Leaf("sa"))),
            "tree"
          )
        )

        assertM(res)(equalTo(Cfg("sa", Set("x"))))
      },
      testM("override default non-empty set with empty set") {
        case class Cfg(a: String, b: Set[String])

        val cCfg = (string("a") zip set("b")(string).default(Set("x"))).to[Cfg]

        val res = read(
          cCfg from ConfigSource.fromPropertyTree(
            Record(Map("a" -> Leaf("sa"), "b" -> Sequence(Nil))),
            "tree"
          )
        )

        assertM(res)(equalTo(Cfg("sa", Set.empty)))
      },
      testM("distinguish set from scalar left") {
        case class Cfg(a: String, b: Either[Set[String], String])

        val cCfg = (string("a") zip nested("b")(set(string).orElseEither(string))).to[Cfg]

        val res = read(
          cCfg from ConfigSource.fromPropertyTree(
            Record(Map("a" -> Leaf("sa"), "b" -> Sequence(Leaf("v") :: Nil))),
            "tree"
          )
        )

        assertM(res)(equalTo(Cfg("sa", Left(Set("v")))))
      },
      testM("distinguish set from scalar right") {
        case class Cfg(a: String, b: Either[String, Set[String]])

        val cCfg = (string("a") zip nested("b")(string.orElseEither(set(string)))).to[Cfg]

        val res = read(
          cCfg from ConfigSource.fromPropertyTree(
            Record(Map("a" -> Leaf("sa"), "b" -> Sequence(Leaf("v") :: Nil))),
            "tree"
          )
        )

        assertM(res)(equalTo(Cfg("sa", Right(Set("v")))))
      },
      testM("distinguish scalar from set left") {
        case class Cfg(a: String, b: Either[String, Set[String]])

        val cCfg = (string("a") zip nested("b")(string.orElseEither(set(string)))).to[Cfg]

        val res = read(
          cCfg from ConfigSource.fromPropertyTree(
            Record(Map("a" -> Leaf("sa"), "b" -> Leaf("v"))),
            "tree"
          )
        )

        assertM(res)(equalTo(Cfg("sa", Left("v"))))
      },
      testM("distinguish scalar from set right") {
        case class Cfg(a: String, b: Either[Set[String], String])

        val cCfg = (string("a") zip nested("b")(set(string).orElseEither(string))).to[Cfg]

        val res = read(
          cCfg from ConfigSource.fromPropertyTree(
            Record(Map("a" -> Leaf("sa"), "b" -> Leaf("v"))).leafNotASequence,
            "tree"
          )
        )

        assertM(res)(equalTo(Cfg("sa", Right("v"))))
      },
      testM("read set as scalar") {
        case class Cfg(a: String, b: String)

        val cCfg = (string("a") zip head("b")(string)).to[Cfg]

        val res = read(
          cCfg from ConfigSource.fromPropertyTree(
            Record(Map("a" -> Leaf("sa"), "b" -> Sequence(Leaf("v1") :: Leaf("v2") :: Nil))),
            "tree"
          )
        )

        assertM(res)(equalTo(Cfg("sa", "v1")))
      },
      testM("read single key objects in nested sets") {
        case class Cfg(a: String, b: Set[Set[String]])

        val cCfg = (string("a") zip set("b")(set(string("c")))).to[Cfg]

        val res = read(
          cCfg from ConfigSource.fromPropertyTree(
            Record(
              Map(
                "a" -> Leaf("sa"),
                "b" -> Sequence[String, String](
                  Sequence(Record(Map("c" -> Leaf("v1"))) :: Nil) ::
                    Sequence(Nil) ::
                    Sequence(Record(Map("c" -> Leaf("v2"))) :: Record(Map("c" -> Leaf("v3"))) :: Nil) ::
                    Nil
                )
              )
            ),
            "tree"
          )
        )

        assertM(res)(equalTo(Cfg("sa", Set(Set("v1"), Set(), Set("v2", "v3")))))
      },
      testM("collect errors from set elements") {
        case class Cfg(a: String, b: Set[String])

        val cCfg = (string("a") zip nested("b")(set(string))).to[Cfg]

        val res = read(
          cCfg from ConfigSource.fromPropertyTree(
            Record(
              Map("a" -> Leaf("sa"), "b" -> Sequence(Record[String, String](Map.empty) :: Sequence(Nil) :: Nil))
            ),
            "tree"
          )
        )

        assertM(res.either)(isLeft(hasField[ReadError[String], Int]("size", _.size, equalTo(2))))
      },
      testM("fails if contains duplicate values") {
        case class Cfg(a: String, b: Set[String])

        val cCfg = (string("a") zip nested("b")(set(string))).to[Cfg]

        val res = read(
          cCfg from ConfigSource.fromPropertyTree(
            Record(
              Map("a" -> Leaf("sa"), "b" -> Sequence(Leaf("v1") :: Leaf("v2") :: Leaf("v1") :: Nil))
            ),
            "tree"
          )
        )

        assertM(res.either)(
          isLeft(
            equalTo(
              ZipErrors(List(ConversionError(List(Key("b")), "Duplicated values found")))
            )
          )
        )
      },
      testM("fails if nested set contains duplicates") {
        case class Cfg(a: String, b: Set[Set[String]])

        val cCfg = (string("a") zip set("b")(set(string("c")))).to[Cfg]

        val res = read(
          cCfg from ConfigSource.fromPropertyTree(
            Record(
              Map(
                "a" -> Leaf("sa"),
                "b" -> Sequence[String, String](
                  Sequence(Record(Map("c" -> Leaf("v1"))) :: Nil) ::
                    Sequence(
                      Record(Map("c" -> Leaf("v2"))) :: Record(Map("c" -> Leaf("v3"))) :: Record(
                        Map("c" -> Leaf("v2"))
                      ) :: Nil
                    ) ::
                    Nil
                )
              )
            ),
            "tree"
          )
        )

        val expected: ReadError[String] =
          ZipErrors(List(ListErrors(List(ConversionError(List(Key("b"), Index(1)), "Duplicated values found")))))

        assertM(res.either)(isLeft(equalTo(expected)))
      },
      testM("accumulates all errors") {
        case class CfgA(a1: Boolean, a2: Int)
        case class Cfg(a: Set[CfgA], b: Set[Int])

        val cCfgA = (boolean("a1") zip int("a2")).to[CfgA]
        val cCfg  = (nested("a")(set(cCfgA)) zip nested("b")(set(int))).to[Cfg]

        val res                         = read(
          cCfg from ConfigSource.fromPropertyTree(
            Record[String, String](
              Map(
                "a" -> PropertyTree.Sequence(
                  List(
                    Record(
                      Map("a1" -> Leaf("true"), "a2" -> Leaf("lorem ipsum"))
                    ),
                    Record(
                      Map("a1" -> Leaf("true"), "a2" -> Leaf("1"))
                    ),
                    Record(
                      Map("a1" -> Leaf("false"))
                    ),
                    Record(
                      Map("a2" -> Leaf("2"))
                    )
                  )
                ),
                "b" -> Sequence(Leaf("2") :: Leaf("one") :: Leaf("2") :: Nil)
              )
            ),
            "tree"
          )
        )
        val expected: ReadError[String] =
          ZipErrors(
            List(
              ZipErrors(
                List(
                  MissingValue(List(Key("a"), Index(3), Key("a1"))),
                  MissingValue(List(Key("a"), Index(2), Key("a2"))),
                  FormatError(
                    List(Key("a"), Index(0), Key("a2")),
                    "Provided value is lorem ipsum, expecting the type int",
                    List("value of type int")
                  )
                )
              ),
              ZipErrors(
                List(
                  FormatError(
                    List(Key("b"), Index(1)),
                    "Provided value is one, expecting the type int",
                    List("value of type int")
                  )
                )
              )
            )
          )

        assertM(res.either)(isLeft(hasField[ReadError[String], Int]("size", _.size, equalTo(expected.size))))
      },
      testM("accumulates all errors - pretty print") {
        case class CfgA(a1: Boolean, a2: Int)
        case class Cfg(a: Set[CfgA], b: Set[Int])

        val cCfgA = (boolean("a1") zip int("a2")).to[CfgA]
        val cCfg  = (nested("a")(set(cCfgA)) zip nested("b")(set(int))).to[Cfg]

        val res                         = read(
          cCfg from ConfigSource.fromPropertyTree(
            Record[String, String](
              Map(
                "a" -> PropertyTree.Sequence(
                  List(
                    Record(
                      Map("a1" -> Leaf("true"), "a2" -> Leaf("lorem ipsum"))
                    ),
                    Record(
                      Map("a1" -> Leaf("true"), "a2" -> Leaf("1"))
                    ),
                    Record(
                      Map("a1" -> Leaf("false"))
                    ),
                    Record(
                      Map("a2" -> Leaf("2"))
                    )
                  )
                ),
                "b" -> Sequence(Leaf("2") :: Leaf("one") :: Leaf("2") :: Nil)
              )
            ),
            "tree"
          )
        )
        val expected: ReadError[String] =
          ZipErrors(
            List(
              ZipErrors(
                List(
                  MissingValue(List(Key("a"), Index(3), Key("a1")), List("value of type boolean")),
                  MissingValue(List(Key("a"), Index(2), Key("a2")), List("value of type int")),
                  FormatError(
                    List(Key("a"), Index(0), Key("a2")),
                    "Provided value is lorem ipsum, expecting the type int",
                    List("value of type int")
                  )
                )
              ),
              ZipErrors(
                List(
                  FormatError(
                    List(Key("b"), Index(1)),
                    "Provided value is one, expecting the type int",
                    List("value of type int")
                  )
                )
              )
            )
          )

        assertM(res.mapError(_.prettyPrint()).either)(isLeft(equalTo(expected.prettyPrint())))
      }
    )
}
