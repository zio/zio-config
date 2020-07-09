package zio.config

import zio.config.ConfigDescriptor._
import zio.config.PropertyTree.{ Leaf, Record, Sequence }
import zio.config.ReadError.Step.{ Index, Key }
import zio.config.ReadError.{ ConversionError, FormatError, ListErrors, MissingValue, ZipErrors }
import zio.test.Assertion._
import zio.test._

object SetTest
    extends BaseSpec(
      suite("SetsTest")(
        test("read empty set") {
          case class Cfg(a: String, b: Set[String])

          val cCfg = (string("a") |@| set("b")(string))(Cfg, Cfg.unapply)

          val res = read(
            cCfg from ConfigSource.fromPropertyTree(
              Record(Map("a" -> Leaf("sa"), "b" -> Sequence(Nil))),
              "tree",
              LeafForSequence.Valid
            )
          )

          assert(res)(isRight(equalTo(Cfg("sa", Set.empty))))
        },
        test("read nested sets") {
          case class Cfg(a: String, b: Set[Set[String]])

          val cCfg = (string("a") |@| set("b")(set(string)))(Cfg, Cfg.unapply)

          val res =
            read(
              cCfg from ConfigSource.fromPropertyTree(
                Record(Map("a" -> Leaf("sa"), "b" -> Sequence(Sequence(Nil) :: Nil))),
                "tree",
                LeafForSequence.Valid
              )
            )

          assert(res)(isRight(equalTo(Cfg("sa", Set(Set.empty)))))
        },
        test("read absent optional sets") {
          case class Cfg(a: String, b: Option[Set[String]])

          val cCfg = (string("a") |@| set("b")(string).optional)(Cfg, Cfg.unapply)

          val res =
            read(
              cCfg from ConfigSource.fromPropertyTree(
                Record(Map("a" -> Leaf("sa"))),
                "tree",
                LeafForSequence.Valid
              )
            )

          assert(res)(isRight(equalTo(Cfg("sa", None))))
        },
        test("read present optional empty sets") {
          case class Cfg(a: String, b: Option[Set[String]])

          val cCfg = (string("a") |@| set("b")(string).optional)(Cfg, Cfg.unapply)

          val res =
            read(
              cCfg from ConfigSource.fromPropertyTree(
                Record(Map("a" -> Leaf("sa"), "b" -> Sequence(Nil))),
                "tree",
                LeafForSequence.Valid
              )
            )

          assert(res)(isRight(equalTo(Cfg("sa", Some(Set.empty)))))
        },
        test("use default value for absent set") {
          case class Cfg(a: String, b: Set[String])

          val cCfg = (string("a") |@| set("b")(string).default(Set("x")))(Cfg, Cfg.unapply)

          val res = read(
            cCfg from ConfigSource.fromPropertyTree(
              Record(Map("a" -> Leaf("sa"))),
              "tree",
              LeafForSequence.Valid
            )
          )

          assert(res)(isRight(equalTo(Cfg("sa", Set("x")))))
        },
        test("override default non-empty set with empty set") {
          case class Cfg(a: String, b: Set[String])

          val cCfg = (string("a") |@| set("b")(string).default(Set("x")))(Cfg, Cfg.unapply)

          val res = read(
            cCfg from ConfigSource.fromPropertyTree(
              Record(Map("a" -> Leaf("sa"), "b" -> Sequence(Nil))),
              "tree",
              LeafForSequence.Valid
            )
          )

          assert(res)(isRight(equalTo(Cfg("sa", Set.empty))))
        },
        test("distinguish set from scalar left") {
          case class Cfg(a: String, b: Either[Set[String], String])

          val cCfg = (string("a") |@| nested("b")(set(string).orElseEither(string)))(Cfg, Cfg.unapply)

          val res = read(
            cCfg from ConfigSource.fromPropertyTree(
              Record(Map("a" -> Leaf("sa"), "b" -> Sequence(Leaf("v") :: Nil))),
              "tree",
              LeafForSequence.Valid
            )
          )

          assert(res)(isRight(equalTo(Cfg("sa", Left(Set("v"))))))
        },
        test("distinguish set from scalar right") {
          case class Cfg(a: String, b: Either[String, Set[String]])

          val cCfg = (string("a") |@| nested("b")(string.orElseEither(set(string))))(Cfg, Cfg.unapply)

          val res = read(
            cCfg from ConfigSource.fromPropertyTree(
              Record(Map("a" -> Leaf("sa"), "b" -> Sequence(Leaf("v") :: Nil))),
              "tree",
              LeafForSequence.Valid
            )
          )

          assert(res)(isRight(equalTo(Cfg("sa", Right(Set("v"))))))
        },
        test("distinguish scalar from set left") {
          case class Cfg(a: String, b: Either[String, Set[String]])

          val cCfg = (string("a") |@| nested("b")(string.orElseEither(set(string))))(Cfg, Cfg.unapply)

          val res = read(
            cCfg from ConfigSource.fromPropertyTree(
              Record(Map("a" -> Leaf("sa"), "b" -> Leaf("v"))),
              "tree",
              LeafForSequence.Valid
            )
          )

          assert(res)(isRight(equalTo(Cfg("sa", Left("v")))))
        },
        test("distinguish scalar from set right") {
          case class Cfg(a: String, b: Either[Set[String], String])

          val cCfg = (string("a") |@| nested("b")(set(string).orElseEither(string)))(Cfg, Cfg.unapply)

          val res = read(
            cCfg from ConfigSource.fromPropertyTree(
              Record(Map("a" -> Leaf("sa"), "b" -> Leaf("v"))),
              "tree",
              LeafForSequence.Invalid
            )
          )

          assert(res)(isRight(equalTo(Cfg("sa", Right("v")))))
        },
        test("read set as scalar") {
          case class Cfg(a: String, b: String)

          val cCfg = (string("a") |@| head("b")(string))(Cfg, Cfg.unapply)

          val res = read(
            cCfg from ConfigSource.fromPropertyTree(
              Record(Map("a" -> Leaf("sa"), "b" -> Sequence(Leaf("v1") :: Leaf("v2") :: Nil))),
              "tree",
              LeafForSequence.Valid
            )
          )

          assert(res)(isRight(equalTo(Cfg("sa", "v1"))))
        },
        test("read single key objects in nested sets") {
          case class Cfg(a: String, b: Set[Set[String]])

          val cCfg = (string("a") |@| set("b")(set(string("c"))))(Cfg, Cfg.unapply)

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
              "tree",
              LeafForSequence.Valid
            )
          )

          assert(res)(isRight(equalTo(Cfg("sa", Set(Set("v1"), Set(), Set("v2", "v3"))))))
        },
        test("collect errors from set elements") {
          case class Cfg(a: String, b: Set[String])

          val cCfg = (string("a") |@| nested("b")(set(string)))(Cfg, Cfg.unapply)

          val res = read(
            cCfg from ConfigSource.fromPropertyTree(
              Record(
                Map("a" -> Leaf("sa"), "b" -> Sequence(Record[String, String](Map.empty) :: Sequence(Nil) :: Nil))
              ),
              "tree",
              LeafForSequence.Valid
            )
          )

          assert(res)(isLeft(hasField("size", _.size, equalTo(2))))
        },
        test("fails if contains duplicate values") {
          case class Cfg(a: String, b: Set[String])

          val cCfg = (string("a") |@| nested("b")(set(string)))(Cfg, Cfg.unapply)

          val res = read(
            cCfg from ConfigSource.fromPropertyTree(
              Record(
                Map("a" -> Leaf("sa"), "b" -> Sequence(Leaf("v1") :: Leaf("v2") :: Leaf("v1") :: Nil))
              ),
              "tree",
              LeafForSequence.Valid
            )
          )

          assert(res)(
            isLeft(
              equalTo(
                ZipErrors(List(ConversionError(List(Key("b")), "Duplicated values found")))
              )
            )
          )
        },
        test("fails if nested set contains duplicates") {
          case class Cfg(a: String, b: Set[Set[String]])

          val cCfg = (string("a") |@| set("b")(set(string("c"))))(Cfg, Cfg.unapply)

          val res = read(
            cCfg from ConfigSource.fromPropertyTree(
              Record(
                Map(
                  "a" -> Leaf("sa"),
                  "b" -> Sequence[String, String](
                    Sequence(Record(Map("c" -> Leaf("v1"))) :: Nil) ::
                      Sequence(
                        Record(Map("c" -> Leaf("v2"))) :: Record(Map("c" -> Leaf("v3"))) :: Record(
                          Map("c"      -> Leaf("v2"))
                        ) :: Nil
                      ) ::
                      Nil
                  )
                )
              ),
              "tree",
              LeafForSequence.Valid
            )
          )

          val expected: ReadError[String] =
            ZipErrors(List(ListErrors(List(ConversionError(List(Key("b"), Index(1)), "Duplicated values found")))))

          assert(res)(isLeft(equalTo(expected)))
        },
        test("accumulates all errors") {
          case class CfgA(a1: Boolean, a2: Int)
          case class Cfg(a: Set[CfgA], b: Set[Int])

          val cCfgA = (boolean("a1") |@| int("a2"))(CfgA, CfgA.unapply)
          val cCfg  = (nested("a")(set(cCfgA)) |@| nested("b")(set(int)))(Cfg, Cfg.unapply)

          val res = read(
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
              "tree",
              LeafForSequence.Valid
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
                      "Provided value is lorem ipsum, expecting the type int"
                    )
                  )
                ),
                ZipErrors(
                  List(
                    FormatError(List(Key("b"), Index(1)), "Provided value is one, expecting the type int")
                  )
                )
              )
            )

          assert(res)(isLeft(hasField("size", _.size, equalTo(expected.size))))
        },
        test("accumulates all errors - pretty print") {
          case class CfgA(a1: Boolean, a2: Int)
          case class Cfg(a: Set[CfgA], b: Set[Int])

          val cCfgA = (boolean("a1") |@| int("a2"))(CfgA, CfgA.unapply)
          val cCfg  = (nested("a")(set(cCfgA)) |@| nested("b")(set(int)))(Cfg, Cfg.unapply)

          val res = read(
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
              "tree",
              LeafForSequence.Valid
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
                      "Provided value is lorem ipsum, expecting the type int"
                    )
                  )
                ),
                ZipErrors(
                  List(FormatError(List(Key("b"), Index(1)), "Provided value is one, expecting the type int"))
                )
              )
            )

          assert(res.left.map(_.prettyPrint()))(isLeft(equalTo(expected.prettyPrint())))
        }
      )
    )
