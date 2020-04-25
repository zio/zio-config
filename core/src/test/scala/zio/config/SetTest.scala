package zio.config

import zio.config.ConfigDescriptor._
import zio.config.PropertyTree.{ Leaf, Record, Sequence }
import zio.config.ReadError.{ AndErrors, ConversionError, MissingValue, OrErrors }
import zio.config.ReadError.Step.{ Index, Key }
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
              "tree"
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
                "tree"
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
                "tree"
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
                "tree"
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
              "tree"
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
              "tree"
            )
          )

          assert(res)(isRight(equalTo(Cfg("sa", Set.empty))))
        },
        test("distinguish set from scalar left") {
          case class Cfg(a: String, b: Either[Set[String], String])

          val cCfg = (string("a") |@| nested("b")(setStrict(string).orElseEither(string)))(Cfg, Cfg.unapply)

          val res = read(
            cCfg from ConfigSource.fromPropertyTree(
              Record(Map("a" -> Leaf("sa"), "b" -> Sequence(Leaf("v") :: Nil))),
              "tree"
            )
          )

          assert(res)(isRight(equalTo(Cfg("sa", Left(Set("v"))))))
        },
        test("distinguish set from scalar right") {
          case class Cfg(a: String, b: Either[String, Set[String]])

          val cCfg = (string("a") |@| nested("b")(string.orElseEither(setStrict(string))))(Cfg, Cfg.unapply)

          val res = read(
            cCfg from ConfigSource.fromPropertyTree(
              Record(Map("a" -> Leaf("sa"), "b" -> Sequence(Leaf("v") :: Nil))),
              "tree"
            )
          )

          assert(res)(isRight(equalTo(Cfg("sa", Right(Set("v"))))))
        },
        test("distinguish scalar from set left") {
          case class Cfg(a: String, b: Either[String, Set[String]])

          val cCfg = (string("a") |@| nested("b")(string.orElseEither(setStrict(string))))(Cfg, Cfg.unapply)

          val res = read(
            cCfg from ConfigSource.fromPropertyTree(
              Record(Map("a" -> Leaf("sa"), "b" -> Leaf("v"))),
              "tree"
            )
          )

          assert(res)(isRight(equalTo(Cfg("sa", Left("v")))))
        },
        test("distinguish scalar from set right") {
          case class Cfg(a: String, b: Either[Set[String], String])

          val cCfg = (string("a") |@| nested("b")(setStrict(string).orElseEither(string)))(Cfg, Cfg.unapply)

          val res = read(
            cCfg from ConfigSource.fromPropertyTree(
              Record(Map("a" -> Leaf("sa"), "b" -> Leaf("v"))),
              "tree"
            )
          )

          assert(res)(isRight(equalTo(Cfg("sa", Right("v")))))
        },
        test("read scalar as set") {
          case class Cfg(a: String, b: Set[String])

          val cCfg = (string("a") |@| set("b")(string))(Cfg, Cfg.unapply)

          val res = read(
            cCfg from ConfigSource.fromPropertyTree(
              Record(Map("a" -> Leaf("sa"), "b" -> Leaf("v"))),
              "tree"
            )
          )

          assert(res)(isRight(equalTo(Cfg("sa", Set("v")))))
        },
        test("read set as scalar") {
          case class Cfg(a: String, b: String)

          val cCfg = (string("a") |@| head("b")(string))(Cfg, Cfg.unapply)

          val res = read(
            cCfg from ConfigSource.fromPropertyTree(
              Record(Map("a" -> Leaf("sa"), "b" -> Sequence(Leaf("v1") :: Leaf("v2") :: Nil))),
              "tree"
            )
          )

          assert(res)(isRight(equalTo(Cfg("sa", "v1"))))
        },
        test("read single key objects in nested sets") {
          case class Cfg(a: String, b: Set[Set[String]])

          val cCfg = (string("a") |@| set("b")(setStrict(string("c"))))(Cfg, Cfg.unapply)

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

          assert(res)(isRight(equalTo(Cfg("sa", Set(Set("v1"), Set(), Set("v2", "v3"))))))
        },
        test("collect errors from set elements") {
          case class Cfg(a: String, b: Set[String])

          val cCfg = (string("a") |@| nested("b")(setStrict(string)))(Cfg, Cfg.unapply)

          val res = read(
            cCfg from ConfigSource.fromPropertyTree(
              Record(
                Map("a" -> Leaf("sa"), "b" -> Sequence(Record[String, String](Map.empty) :: Sequence(Nil) :: Nil))
              ),
              "tree"
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
              "tree"
            )
          )

          assert(res)(isLeft(equalTo(ConversionError[String](Key("b") :: Nil, "Duplicated values found"))))
        },
        test("fails if nested set contains duplicates") {
          case class Cfg(a: String, b: Set[Set[String]])

          val cCfg = (string("a") |@| set("b")(setStrict(string("c"))))(Cfg, Cfg.unapply)

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
              "tree"
            )
          )

          val expected: ReadError[String] = OrErrors(
            List(
              AndErrors(List(ConversionError(List(Key("b"), Index(1)), "Duplicated values found"))),
              AndErrors(
                List(MissingValue(List(Key("b"), Index(1), Key("c"))), MissingValue(List(Key("b"), Index(0), Key("c"))))
              )
            )
          )
          assert(res)(isLeft(equalTo(expected)))
        }
      )
    )
