package zio.config

import zio.config.ConfigDescriptor.{ Source => _, _ }
import zio.config.PropertyTree.{ Leaf, Record, Sequence }
import zio.test.Assertion._
import zio.test._

object ListsCornerCasesTest
    extends BaseSpec(
      suite("ListsCornerCasesTest")(
        test("read empty list") {
          case class Cfg(a: String, b: List[String])

          val cCfg = (string("a") |@| list("b")(string))(Cfg, Cfg.unapply)

          val res = read(cCfg from ConfigSource(Record(Map("a" -> Leaf("sa"), "b" -> Sequence(Nil))), Nil))

          assert(res)(isRight(equalTo(Cfg("sa", Nil))))
        },
        test("read nested lists") {
          case class Cfg(a: String, b: List[List[String]])

          val cCfg = (string("a") |@| list("b")(list(string)))(Cfg, Cfg.unapply)

          val res =
            read(cCfg from ConfigSource(Record(Map("a" -> Leaf("sa"), "b" -> Sequence(Sequence(Nil) :: Nil))), Nil))

          assert(res)(isRight(equalTo(Cfg("sa", Nil :: Nil))))
        },
        test("read absent optional lists") {
          case class Cfg(a: String, b: Option[List[String]])

          val cCfg = (string("a") |@| list("b")(string).optional)(Cfg, Cfg.unapply)

          val res =
            read(cCfg from ConfigSource(Record(Map("a" -> Leaf("sa"))), Nil))

          assert(res)(isRight(equalTo(Cfg("sa", None))))
        },
        test("read present optional empty lists") {
          case class Cfg(a: String, b: Option[List[String]])

          val cCfg = (string("a") |@| list("b")(string).optional)(Cfg, Cfg.unapply)

          val res =
            read(cCfg from ConfigSource(Record(Map("a" -> Leaf("sa"), "b" -> Sequence(Nil))), Nil))

          assert(res)(isRight(equalTo(Cfg("sa", Some(Nil)))))
        },
        test("use default value for absent list") {
          case class Cfg(a: String, b: List[String])

          val cCfg = (string("a") |@| list("b")(string).default("x" :: Nil))(Cfg, Cfg.unapply)

          val res = read(cCfg from ConfigSource(Record(Map("a" -> Leaf("sa"))), Nil))

          assert(res)(isRight(equalTo(Cfg("sa", "x" :: Nil))))
        },
        test("override default non-empty list with empty list") {
          case class Cfg(a: String, b: List[String])

          val cCfg = (string("a") |@| list("b")(string).default("x" :: Nil))(Cfg, Cfg.unapply)

          val res = read(cCfg from ConfigSource(Record(Map("a" -> Leaf("sa"), "b" -> Sequence(Nil))), Nil))

          assert(res)(isRight(equalTo(Cfg("sa", Nil))))
        },
        test("distinguish list from scalar left") {
          case class Cfg(a: String, b: Either[List[String], String])

          val cCfg = (string("a") |@| nested("b")(listStrict(string).orElseEither(string)))(Cfg, Cfg.unapply)

          val res = read(cCfg from ConfigSource(Record(Map("a" -> Leaf("sa"), "b" -> Sequence(Leaf("v") :: Nil))), Nil))

          assert(res)(isRight(equalTo(Cfg("sa", Left("v" :: Nil)))))
        },
        test("distinguish list from scalar right") {
          case class Cfg(a: String, b: Either[String, List[String]])

          val cCfg = (string("a") |@| nested("b")(string.orElseEither(listStrict(string))))(Cfg, Cfg.unapply)

          val res = read(cCfg from ConfigSource(Record(Map("a" -> Leaf("sa"), "b" -> Sequence(Leaf("v") :: Nil))), Nil))

          assert(res)(isRight(equalTo(Cfg("sa", Right("v" :: Nil)))))
        },
        test("distinguish scalar from list left") {
          case class Cfg(a: String, b: Either[String, List[String]])

          val cCfg = (string("a") |@| nested("b")(string.orElseEither(listStrict(string))))(Cfg, Cfg.unapply)

          val res = read(cCfg from ConfigSource(Record(Map("a" -> Leaf("sa"), "b" -> Leaf("v"))), Nil))

          assert(res)(isRight(equalTo(Cfg("sa", Left("v")))))
        },
        test("distinguish scalar from list right") {
          case class Cfg(a: String, b: Either[List[String], String])

          val cCfg = (string("a") |@| nested("b")(listStrict(string).orElseEither(string)))(Cfg, Cfg.unapply)

          val res = read(cCfg from ConfigSource(Record(Map("a" -> Leaf("sa"), "b" -> Leaf("v"))), Nil))

          assert(res)(isRight(equalTo(Cfg("sa", Right("v")))))
        },
        test("read scalar as list") {
          case class Cfg(a: String, b: List[String])

          val cCfg = (string("a") |@| list("b")(string))(Cfg, Cfg.unapply)

          val res = read(cCfg from ConfigSource(Record(Map("a" -> Leaf("sa"), "b" -> Leaf("v"))), Nil))

          assert(res)(isRight(equalTo(Cfg("sa", "v" :: Nil))))
        },
        test("read list as scalar") {
          case class Cfg(a: String, b: String)

          val cCfg = (string("a") |@| first("b")(string))(Cfg, Cfg.unapply)

          val res = read(
            cCfg from ConfigSource(
              Record(Map("a" -> Leaf("sa"), "b" -> Sequence(Leaf("v1") :: Leaf("v2") :: Nil))),
              Nil
            )
          )

          assert(res)(isRight(equalTo(Cfg("sa", "v1"))))
        },
        test("read single key objects in nested lists") {
          case class Cfg(a: String, b: List[List[String]])

          val cCfg = (string("a") |@| list("b")(listStrict(string("c"))))(Cfg, Cfg.unapply)

          val res = read(
            cCfg from ConfigSource(
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
              Nil
            )
          )

          assert(res)(isRight(equalTo(Cfg("sa", List("v1") :: Nil :: List("v2", "v3") :: Nil))))
        },
        test("collect errors from list elements") {
          case class Cfg(a: String, b: List[String])

          val cCfg = (string("a") |@| nested("b")(listStrict(string)))(Cfg, Cfg.unapply)

          val res = read(
            cCfg from ConfigSource(
              Record(
                Map("a" -> Leaf("sa"), "b" -> Sequence(Record[String, String](Map.empty) :: Sequence(Nil) :: Nil))
              ),
              Nil
            )
          )

          assert(res)(isLeft(hasField("size", _.size, equalTo(2))))
        }
      )
    )
