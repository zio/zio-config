package zio.config

import zio.config.ConfigDescriptor._
import zio.config.PropertyTree.{ Leaf, Record, Sequence }
import zio.test.Assertion.{ anything, equalTo, isLeft, isNone, isRight }
import zio.test.{ assert, suite, test }

object MapTest
    extends BaseSpec(
      suite("MapCornerCasesTest")(
        test("map(b)(string(y)) returns the value of y from the values inside map within b.") {
          case class Cfg(a: String, b: Map[String, String])

          val cCfg = (string("a") |@| map("b")(string("c")))(Cfg, Cfg.unapply)

          val res = read(
            cCfg from ConfigSource
              .fromPropertyTree(
                Record(
                  Map("a" -> Leaf("sa"), "b" -> Record(Map("z" -> Record(Map("c" -> Leaf("d"), "f" -> Leaf("h"))))))
                ),
                "tree",
                LeafForSequence.Valid
              )
          )

          assert(res)(isRight(equalTo(Cfg("sa", Map("z" -> "d")))))
        },
        test("map(string(y)) returns the value of y amongst the values of the map as a string.") {
          case class Cfg(a: String, b: Map[String, String])

          val cCfg = (string("a") |@| map("b")(string("c")))(Cfg, Cfg.unapply)

          val res = read(
            cCfg from ConfigSource
              .fromPropertyTree(
                Record(Map("a" -> Leaf("sa"), "b" -> Record(Map("z" -> Record(Map("c" -> Leaf("d"))))))),
                "tree",
                LeafForSequence.Valid
              )
          )

          assert(res)(isRight(equalTo(Cfg("sa", Map("z" -> "d")))))
        },
        test("map(string(y)) returns the value of y from the value of the map.") {
          case class Cfg(a: String, b: Map[String, List[String]])

          val cCfg = (string("a") |@| map("b")(list(string("c"))))(Cfg, Cfg.unapply)

          val res = read(
            cCfg from ConfigSource
              .fromPropertyTree(
                Record(
                  Map("a" -> Leaf("sa"), "b" -> Record(Map("z" -> Sequence(List(Record(Map("c" -> Leaf("d"))))))))
                ),
                "tree",
                LeafForSequence.Valid
              )
          )

          assert(res)(isRight(equalTo(Cfg("sa", Map("z" -> List("d"))))))
        },
        test("list of delimited values from Map.") {
          case class Cfg(a: String, b: List[String])

          val cCfg = (string("a") |@| list("b")(string))(Cfg, Cfg.unapply)

          // also for ConfigSource.fromSystemEnv
          val res = read(
            cCfg from ConfigSource
              .fromMap(
                Map(
                  "a" -> "sa",
                  "b" -> "q,w,e,r,ty,uio"
                ),
                "string map",
                None,
                Some(',')
              )
          )

          assert(res)(isRight(equalTo(Cfg("sa", List("q", "w", "e", "r", "ty", "uio")))))
        },
        test("read empty map") {
          case class Cfg(a: String, b: Map[String, String])

          val cCfg = (string("a") |@| map("b")(string))(Cfg, Cfg.unapply)

          val res = read(
            cCfg from ConfigSource.fromPropertyTree(
              Record(Map("a" -> Leaf("sa"), "b" -> Record(Map.empty[String, PropertyTree[String, String]]))),
              "tree",
              LeafForSequence.Valid
            )
          )

          assert(res)(isRight(equalTo(Cfg("sa", Map.empty))))
        },
        test("read nested maps") {
          case class Cfg(a: String, b: Map[String, Map[String, List[String]]])

          val cCfg = (string("a") |@| map("b")(map(list(string))))(Cfg, Cfg.unapply)

          val res =
            read(
              cCfg from ConfigSource.fromPropertyTree(
                Record(
                  Map("a" -> Leaf("sa"), "b" -> Record(Map("k" -> Record(Map("hello" -> Sequence(Nil))))))
                ),
                "tree",
                LeafForSequence.Valid
              )
            )

          assert(res)(isRight(equalTo(Cfg("sa", Map("k" -> Map("hello" -> Nil))))))
        },
        test("read absent optional map") {
          case class Cfg(b: Option[Map[String, String]])

          val cCfg =
            map("b")(string).optional(Cfg, Cfg.unapply)

          val res =
            read(
              cCfg from ConfigSource
                .fromPropertyTree(Record(Map("a" -> Record(Map("c" -> Leaf("a"))))), "tree", LeafForSequence.Valid)
            )

          assert(res)(isRight(equalTo(Cfg(None))))
        },
        test("read present optional empty map") {
          case class Cfg(a: String, b: Option[Map[String, String]])

          val cCfg =
            (string("a") |@| map("b")(string).optional)(Cfg, Cfg.unapply)

          val res =
            read(
              cCfg from ConfigSource.fromPropertyTree(
                Record(Map("a" -> Leaf("sa"), "b" -> Record(Map.empty[String, PropertyTree[String, String]]))),
                "tree",
                LeafForSequence.Valid
              )
            )

          assert(res)(isRight(equalTo(Cfg("sa", Some(Map.empty[String, String])))))
        },
        test("use default value for absent map") {
          case class Cfg(a: String, b: Map[String, String])

          val cCfg = (string("a") |@| map("b")(string)
            .default(Map("x" -> "y", "z" -> "a")))(Cfg, Cfg.unapply)

          val res = read(
            cCfg from ConfigSource.fromPropertyTree(Record(Map("a" -> Leaf("sa"))), "tree", LeafForSequence.Valid)
          )

          assert(res)(isRight(equalTo(Cfg("sa", Map("x" -> "y", "z" -> "a")))))
        },
        test("override default non-empty map with empty map") {
          case class Cfg(a: String, b: Map[String, String])

          val cCfg = (string("a") |@| map("b")(string)
            .default(Map("x" -> "y")))(Cfg, Cfg.unapply)

          val res = read(
            cCfg from ConfigSource.fromPropertyTree(
              Record(Map("a" -> Leaf("sa"), "b" -> Record(Map.empty[String, PropertyTree[String, String]]))),
              "tree",
              LeafForSequence.Valid
            )
          )

          assert(res)(isRight(equalTo(Cfg("sa", Map.empty[String, String]))))
        },
        test("mapStrict picks map if map exists") {
          case class Cfg(a: String, b: Either[Map[String, String], String])

          val cCfg = (string("a") |@| nested("b")(
            map(string).orElseEither(string)
          ))(Cfg, Cfg.unapply)

          val res =
            read(
              cCfg from ConfigSource.fromPropertyTree(
                Record(
                  Map("a" -> Leaf("sa"), "b" -> Record(Map("k" -> PropertyTree.Leaf("v"))))
                ),
                "tree",
                LeafForSequence.Valid
              )
            )

          assert(res)(isRight(equalTo(Cfg("sa", Left(Map("k" -> "v"))))))
        },
        test("mapStrict picks map over primitives") {
          case class Cfg(a: String, b: Either[String, Map[String, String]])

          val cCfg = (string("a") |@| nested("b")(
            string.orElseEither(map(string))
          ))(Cfg, Cfg.unapply)

          val res =
            read(
              cCfg from ConfigSource.fromPropertyTree(
                Record(
                  Map("a" -> Leaf("sa"), "b" -> Record(Map("x" -> Leaf("v"))))
                ),
                "tree",
                LeafForSequence.Valid
              )
            )

          assert(res)(isRight(equalTo(Cfg("sa", Right(Map("x" -> "v"))))))
        },
        test("mapStrict picks alternative when failed to find map") {
          case class Cfg(a: String, b: Either[String, Map[String, String]])

          val cCfg = (string("a") |@| nested("b")(
            string.orElseEither(map(string))
          ))(Cfg, Cfg.unapply)

          val res = read(
            cCfg from ConfigSource
              .fromPropertyTree(Record(Map("a" -> Leaf("sa"), "b" -> Leaf("v"))), "tree", LeafForSequence.Valid)
          )

          assert(res)(isRight(equalTo(Cfg("sa", Left("v")))))
        },
        test("mapStrict picks alternative on right when failed to find map") {
          case class Cfg(a: String, b: Either[Map[String, String], String])

          val cCfg = (string("a") |@| nested("b")(
            map(string).orElseEither(string)
          ))(Cfg, Cfg.unapply)

          val res = read(
            cCfg from ConfigSource
              .fromPropertyTree(Record(Map("a" -> Leaf("sa"), "b" -> Leaf("v"))), "tree", LeafForSequence.Valid)
          )

          assert(res)(isRight(equalTo(Cfg("sa", Right("v")))))
        },
        test("key doesn't exist in map") {
          val src = ConfigSource.fromPropertyTree(
            PropertyTree.Record(Map("usr" -> PropertyTree.Leaf("v1"))),
            "src",
            LeafForSequence.Valid
          )
          val optional: ConfigDescriptor[Option[Map[String, String]]] = map(string("keyNotExists")).optional
          assert(read(optional from src))(isLeft(anything))
        },
        test("when empty map") {
          val src = ConfigSource.fromPropertyTree(
            PropertyTree.empty,
            "src",
            LeafForSequence.Valid
          )
          val optional: ConfigDescriptor[Option[Map[String, String]]] = map(string("usr")).optional
          assert(read(optional from src))(isRight(isNone))
        }
      )
    )
