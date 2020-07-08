package zio.config

import zio.ZIO
import zio.config.ConfigDescriptor._
import zio.config.ListAndOptionalTestUtils._
import zio.config.PropertyTree.{ Leaf, Record }
import zio.config.helpers._
import zio.test.Assertion._
import zio.test._

object ListAndOptionalTest
    extends BaseSpec(
      suite("List and options")(
        testM("optional write") {
          checkM(genOverallConfig) { p =>
            val actual = ZIO.fromEither(write(cOverallConfig, p)).map(_.flattenString())

            val expected = p.option
              .flatMap(t => t.flatMap(tt => tt.map(ttt => Map("kId" -> singleton(ttt.value)))))
              .getOrElse(Map.empty[String, ::[String]])

            assertM(actual)(equalTo(expected))
          }
        },
        testM("list read") {

          val src =
            ConfigSource.fromPropertyTree(
              Record(
                Map(
                  "list" -> PropertyTree.Sequence(
                    List(
                      Record(
                        Map(
                          "a" -> Leaf("1"),
                          "b" -> Leaf("1"),
                          "c" -> Leaf("1")
                        )
                      ),
                      Record(
                        Map(
                          "a" -> Leaf("2"),
                          "b" -> Leaf("2")
                        )
                      ),
                      Record(
                        Map(
                          "a" -> Leaf("3")
                        )
                      ),
                      Record(
                        Map(
                          "a" -> Leaf("4"),
                          "c" -> Leaf("4")
                        )
                      ),
                      Record(
                        Map(
                          "a" -> Leaf("5"),
                          "b" -> Leaf("5"),
                          "c" -> Leaf("5")
                        )
                      )
                    )
                  )
                )
              ),
              "src",
              LeafForSequence.Valid
            )

          val actual = ZIO.fromEither(read(cListConfig from src))

          val expected = ListConfig(
            List(
              Opt3Config(Id("1"), Some(Id("1")), Some(Id("1"))),
              Opt3Config(Id("2"), Some(Id("2")), None),
              Opt3Config(Id("3"), None, None),
              Opt3Config(Id("4"), None, Some(Id("4"))),
              Opt3Config(Id("5"), Some(Id("5")), Some(Id("5")))
            )
          )

          assertM(actual)(equalTo(expected))
        },
        testM("empty list read") {

          val src =
            ConfigSource.fromPropertyTree(
              Record(Map("list" -> PropertyTree.Sequence[String, String](Nil))),
              "src",
              LeafForSequence.Valid
            )

          val actual = ZIO.fromEither(read(cListConfig from src))

          val expected = ListConfig(Nil)

          assertM(actual)(equalTo(expected))
        },
        test("key doesn't exist in list") {
          val src = ConfigSource.fromPropertyTree(
            PropertyTree.Sequence(List(Record(Map()))),
            "src",
            LeafForSequence.Valid
          )
          val optional: ConfigDescriptor[Option[List[String]]] = list(string("keyNotExists")).optional
          assert(read(optional from src))(isLeft(anything))
        },
        test("when empty list") {
          val src = ConfigSource.fromPropertyTree(
            PropertyTree.empty,
            "src",
            LeafForSequence.Valid
          )
          val optional: ConfigDescriptor[Option[List[String]]] = list(string("usr")).optional
          assert(read(optional from src))(isRight(isNone))
        },
        testM("list write read") {
          checkM(genListConfig) { p =>
            val actual = ZIO.fromEither(write(cListConfig, p).flatMap { tree =>
              read(cListConfig from ConfigSource.fromPropertyTree(tree, "tree", LeafForSequence.Valid))
            })
            assertM(actual)(equalTo(p))
          }
        },
        test("return failure when branch is not defined correctly") {
          val src = ConfigSource.fromPropertyTree(
            Record(
              Map(
                "branches" -> PropertyTree.Sequence[String, String](
                  List(
                    Record(Map("pattern" -> Leaf("master"), "tag"  -> Leaf("true"))),
                    Record(Map("name"    -> Leaf("pull/.*"), "tag" -> Leaf("true")))
                  )
                )
              )
            ),
            "src",
            LeafForSequence.Valid
          )
          case class AppConfig(branches: Option[List[Branch]])
          case class Branch(pattern: String, tag: Boolean)

          val patternDesc = string("pattern")
          val tagDesc     = boolean("tag")

          val branchConfigDesc =
            (
              patternDesc |@|
                tagDesc
            )(Branch.apply, Branch.unapply)

          val appConfigDesc =
            (list("branches")(branchConfigDesc).optional)(AppConfig.apply, AppConfig.unapply)

          assert(read(appConfigDesc from src))(isLeft(anything))
        }
      )
    )

object ListAndOptionalTestUtils {
  final case class OverallConfig(option: Option[Option[Option[Id]]])

  val genOverallConfig =
    Gen.option(genId).map(t => OverallConfig(t.map(t => Option(Option(t)))))

  private def id(path: String) = string(path)(Id.apply, Id.unapply)

  private val cId = id("kId")

  val cOverallConfig =
    cId.optional.optional.optional(OverallConfig.apply, OverallConfig.unapply)

  final case class Opt3Config(a: Id, b: Option[Id], c: Option[Id])
  final case class ListConfig(list: List[Opt3Config])

  val cOpt3Config = (id("a") |@| id("b").optional |@| id("c").optional)(Opt3Config, Opt3Config.unapply)

  val cListConfig = list("list")(cOpt3Config)(ListConfig, ListConfig.unapply)

  val genOpt3Config = for {
    a <- genId
    b <- Gen.option(genId)
    c <- Gen.option(genId)
  } yield Opt3Config(a, b, c)

  val genListConfig = Gen.listOf(genOpt3Config).map(ListConfig)
}
