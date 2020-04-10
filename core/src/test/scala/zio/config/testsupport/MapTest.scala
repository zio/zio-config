package zio.config

import zio.config.ConfigDescriptor.{ Source => _, _ }
import zio.config.PropertyTree.{ Leaf, Record, Sequence }
import zio.test.Assertion._
import zio.test._

object MapTest
  extends BaseSpec(
    suite("MapCornerCasesTest")(
      test("read empty map") {
        case class Cfg(a: String, b: Map[String, String])

        val cCfg = (string("a") |@| map("b")(string))(Cfg, Cfg.unapply)

        val res = read(
          cCfg from ConfigSource.mk(
            Record(Map("a" -> Leaf("sa"), "b" -> Record(Map.empty[String, PropertyTree[String, String]]))),
            Set.empty
          )
        )

        assert(res)(isRight(equalTo(Cfg("sa", Map.empty))))
      },
      test("read nested lists") {
        case class Cfg(a: String, b: Map[String, Map[String, List[String]]])

        val cCfg = (string("a") |@| map("b")(map(list(string))))(Cfg, Cfg.unapply)

        val res =
          read(
            cCfg from ConfigSource.mk(
              Record(
                Map("a" -> Leaf("sa"), "b" -> Record(Map("k" -> Record(Map("hello" -> Sequence(Nil))))))
              ),
              Set.empty
            )
          )

        assert(res)(isRight(equalTo(Cfg("sa", Map("k" -> Map("hello" -> Nil))))))
      },
      test("read absent optional lists") {
        case class Cfg(b: Option[Map[String, String]])

        val cCfg =
          map("b")(string).optional(Cfg, Cfg.unapply)

        val res =
          read(
            cCfg from ConfigSource
              .fromPropertyTree(Record(Map("a" -> Record(Map("c" -> Leaf("a"))))), "tree")
          )

        assert(res)(isRight(equalTo(Cfg(None))))
      },
      test("read present optional empty lists") {
        case class Cfg(a: String, b: Option[List[String]])

        val cCfg =
          (string("a") |@| list("b")(string).optional)(Cfg, Cfg.unapply)

        val res =
          read(
            cCfg from ConfigSource.mk(
              Record(Map("a" -> Leaf("sa"), "b" -> Sequence(Nil))),
              Set.empty
            )
          )

        assert(res)(isRight(equalTo(Cfg("sa", Some(Nil)))))
      },
      test("use default value for absent list") {
        case class Cfg(a: String, b: List[String])

        val cCfg = (string("a") |@| list("b")(string)
          .default("x" :: Nil))(Cfg, Cfg.unapply)

        val res = read(
          cCfg from ConfigSource.mk(Record(Map("a" -> Leaf("sa"))), Set.empty)
        )

        assert(res)(isRight(equalTo(Cfg("sa", "x" :: Nil))))
      },
      test("override default non-empty list with empty list") {
        case class Cfg(a: String, b: List[String])

        val cCfg = (string("a") |@| list("b")(string)
          .default("x" :: Nil))(Cfg, Cfg.unapply)

        val res = read(
          cCfg from ConfigSource.mk(
            Record(Map("a" -> Leaf("sa"), "b" -> Sequence(Nil))),
            Set.empty
          )
        )

        assert(res)(isRight(equalTo(Cfg("sa", Nil))))
      },
      test("distinguish list from scalar left") {
        case class Cfg(a: String, b: Either[List[String], String])

        val cCfg = (string("a") |@| nested("b")(
          listStrict(string).orElseEither(string)
        ))(Cfg, Cfg.unapply)

        val res =
          read(
            cCfg from ConfigSource.mk(
              Record(
                Map("a" -> Leaf("sa"), "b" -> Sequence(Leaf("v") :: Nil))
              ),
              Set.empty
            )
          )

        assert(res)(isRight(equalTo(Cfg("sa", Left("v" :: Nil)))))
      },
      test("distinguish list from scalar right") {
        case class Cfg(a: String, b: Either[String, List[String]])

        val cCfg = (string("a") |@| nested("b")(
          string.orElseEither(listStrict(string))
        ))(Cfg, Cfg.unapply)

        val res =
          read(
            cCfg from ConfigSource.mk(
              Record(
                Map("a" -> Leaf("sa"), "b" -> Sequence(Leaf("v") :: Nil))
              ),
              Set.empty
            )
          )

        assert(res)(isRight(equalTo(Cfg("sa", Right("v" :: Nil)))))
      },
      test("distinguish scalar from list left") {
        case class Cfg(a: String, b: Either[String, List[String]])

        val cCfg = (string("a") |@| nested("b")(
          string.orElseEither(listStrict(string))
        ))(Cfg, Cfg.unapply)

        val res = read(
          cCfg from ConfigSource
            .mk(Record(Map("a" -> Leaf("sa"), "b" -> Leaf("v"))), Set.empty)
        )

        assert(res)(isRight(equalTo(Cfg("sa", Left("v")))))
      },
      test("distinguish scalar from list right") {
        case class Cfg(a: String, b: Either[List[String], String])

        val cCfg = (string("a") |@| nested("b")(
          listStrict(string).orElseEither(string)
        ))(Cfg, Cfg.unapply)

        val res = read(
          cCfg from ConfigSource
            .mk(Record(Map("a" -> Leaf("sa"), "b" -> Leaf("v"))), Set.empty)
        )

        assert(res)(isRight(equalTo(Cfg("sa", Right("v")))))
      },
      test("read scalar as list") {
        case class Cfg(a: String, b: List[String])

        val cCfg = (string("a") |@| list("b")(string))(Cfg, Cfg.unapply)

        val res = read(
          cCfg from ConfigSource
            .mk(Record(Map("a" -> Leaf("sa"), "b" -> Leaf("v"))), Set.empty)
        )

        assert(res)(isRight(equalTo(Cfg("sa", "v" :: Nil))))
      },
      test("read list as scalar") {
        case class Cfg(a: String, b: String)

        val cCfg = (string("a") |@| head("b")(string))(Cfg, Cfg.unapply)

        val res = read(
          cCfg from ConfigSource.mk(
            Record(
              Map(
                "a" -> Leaf("sa"),
                "b" -> Sequence(Leaf("v1") :: Leaf("v2") :: Nil)
              )
            ),
            Set.empty
          )
        )

        assert(res)(isRight(equalTo(Cfg("sa", "v1"))))
      },
      test("read single key objects in nested lists") {
        case class Cfg(a: String, b: List[List[String]])

        val cCfg = (string("a") |@| list("b")(listStrict(string("c"))))(
          Cfg,
          Cfg.unapply
        )

        val res = read(
          cCfg from ConfigSource.mk(
            Record(
              Map(
                "a" -> Leaf("sa"),
                "b" -> Sequence[String, String](
                  Sequence(Record(Map("c" -> Leaf("v1"))) :: Nil) ::
                    Sequence(Nil) ::
                    Sequence(
                      Record(Map("c" -> Leaf("v2"))) :: Record(
                        Map("c"      -> Leaf("v3"))
                      ) :: Nil
                    ) ::
                    Nil
                )
              )
            ),
            Set.empty
          )
        )

        assert(res)(
          isRight(
            equalTo(Cfg("sa", List("v1") :: Nil :: List("v2", "v3") :: Nil))
          )
        )
      },
      test("collect errors from list elements") {
        case class Cfg(a: String, b: List[String])

        val cCfg =
          (string("a") |@| nested("b")(listStrict(string)))(Cfg, Cfg.unapply)

        val res = read(
          cCfg from ConfigSource.mk(
            Record(
              Map(
                "a" -> Leaf("sa"),
                "b" -> Sequence(
                  Record[String, String](Map.empty) :: Sequence(Nil) :: Nil
                )
              )
            ),
            Set.empty
          )
        )

        assert(res)(isLeft(hasField("size", _.size, equalTo(2))))
      }
    )
  )
