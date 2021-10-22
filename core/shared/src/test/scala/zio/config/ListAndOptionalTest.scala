package zio.config

import zio.config.ConfigDescriptor._
import zio.config.ListAndOptionalTestUtils._
import zio.config.PropertyTree.{Leaf, Record}
import zio.config.helpers._
import zio.test.Assertion._
import zio.test._
import zio.{Has, Random, ZIO}

object ListAndOptionalTest extends BaseSpec {

  val spec: Spec[Has[TestConfig] with Has[Random] with Has[Sized], TestFailure[
    String
  ], TestSuccess] =
    suite("List and options")(
      test("optional write") {
        check(genOverallConfig) { p =>
          val actual = ZIO.fromEither(write(cOverallConfig, p)).map(_.flattenString())

          val expected = p.option
            .flatMap(t => t.flatMap(tt => tt.map(ttt => Map("kId" -> singleton(ttt.value)))))
            .getOrElse(Map.empty[String, ::[String]])

          assertM(actual)(equalTo(expected))
        }
      },
      test("list read") {

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

        val actual = ZIO.fromEither(read(cListConfig from src)).mapError(_.getMessage)

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
      test("empty list read") {

        val src =
          ConfigSource.fromPropertyTree(
            Record(Map("list" -> PropertyTree.Sequence[String, String](Nil))),
            "src",
            LeafForSequence.Valid
          )

        val actual = ZIO.fromEither(read(cListConfig from src)).mapError(_.getMessage)

        val expected = ListConfig(Nil)

        assertM(actual)(equalTo(expected))
      },
      test("key doesn't exist in list") {
        val src                                              = ConfigSource.fromPropertyTree(
          PropertyTree.Sequence(List(Record(Map()))),
          "src",
          LeafForSequence.Valid
        )
        val optional: ConfigDescriptor[Option[List[String]]] = list(string("keyNotExists")).optional
        assert(read(optional from src))(isLeft(anything))
      },
      test("when empty list") {
        val src                                              = ConfigSource.fromPropertyTree(
          PropertyTree.empty,
          "src",
          LeafForSequence.Valid
        )
        val optional: ConfigDescriptor[Option[List[String]]] = list(string("usr")).optional
        assert(read(optional from src))(isRight(isNone))
      },
      test("list write read") {
        check(genListConfig) { p =>
          val actual = ZIO.fromEither(write(cListConfig, p)).flatMap { tree =>
            ZIO
              .fromEither(read(cListConfig from ConfigSource.fromPropertyTree(tree, "tree", LeafForSequence.Valid)))
              .mapError(_.getMessage)
          }
          assertM(actual)(equalTo(p))
        }
      },
      test("return failure when branch is not defined correctly") {
        val src = ConfigSource.fromPropertyTree(
          Record(
            Map(
              "branches" -> PropertyTree.Sequence[String, String](
                List(
                  Record(Map("pattern" -> Leaf("master"), "tag" -> Leaf("true"))),
                  Record(Map("name" -> Leaf("pull/.*"), "tag" -> Leaf("true")))
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
          ).to[Branch]

        val appConfigDesc =
          (list("branches")(branchConfigDesc).optional).to[AppConfig]

        assert(read(appConfigDesc from src))(isLeft(anything))
      },
      test("listOrSingleton on list input") {
        val src =
          ConfigSource.fromPropertyTree(
            Record(
              Map(
                "list" -> PropertyTree.Sequence[String, String](
                  List(PropertyTree.Leaf("x"), PropertyTree.Leaf("y"))
                )
              )
            ),
            "src",
            LeafForSequence.Valid
          )

        val config   = listOrSingleton("list")(string)
        val actual   = ZIO.fromEither(read(config from src)).mapError(_.getMessage)
        val expected = List("x", "y")

        assertM(actual)(equalTo(expected))
      },
      test("listOrSingleton on singleton input") {
        val src =
          ConfigSource.fromPropertyTree(
            Record(Map("list" -> PropertyTree.Leaf("x"))),
            "src",
            LeafForSequence.Invalid // Note that with LeafForSequence.Valid, this is accepted as a list (not the 'orElse singleton')
          )

        val config   = listOrSingleton("list")(string)
        val actual   = ZIO.fromEither(read(config from src)).mapError(_.getMessage)
        val expected = List("x")

        assertM(actual)(equalTo(expected))
      }
    )
}

object ListAndOptionalTestUtils {
  final case class OverallConfig(option: Option[Option[Option[Id]]])

  val genOverallConfig: Gen[Has[Random], OverallConfig] =
    Gen.option(genId).map(t => OverallConfig(t.map(t => Option(Option(t)))))

  private def id(path: String) = string(path).to[Id]

  private val cId = id("kId")

  val cOverallConfig: ConfigDescriptor[OverallConfig] =
    cId.optional.optional.optional.to[OverallConfig]

  final case class Opt3Config(a: Id, b: Option[Id], c: Option[Id])
  final case class ListConfig(list: List[Opt3Config])

  val cOpt3Config: ConfigDescriptor[Opt3Config] = (id("a") |@| id("b").optional |@| id("c").optional).to[Opt3Config]

  val cListConfig: ConfigDescriptor[ListConfig] = list("list")(cOpt3Config).to[ListConfig]

  val genOpt3Config: Gen[Has[Random], Opt3Config] = for {
    a <- genId
    b <- Gen.option(genId)
    c <- Gen.option(genId)
  } yield Opt3Config(a, b, c)

  val genListConfig: Gen[Has[Random] with Has[Sized], ListConfig] = Gen.listOf(genOpt3Config).map(ListConfig.apply)
}
