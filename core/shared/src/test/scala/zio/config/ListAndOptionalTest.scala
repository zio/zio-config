package zio.config

import zio.config.ConfigDescriptor._
import zio.config.ListAndOptionalTestUtils._
import zio.config.PropertyTree.{Leaf, Record}
import zio.config.helpers._
import zio.random.Random
import zio.test.Assertion._
import zio.test._
import zio.{Has, ZIO}

object ListAndOptionalTest extends BaseSpec {

  val spec: Spec[Has[TestConfig.Service] with Has[Random.Service] with Has[Sized.Service], TestFailure[
    String
  ], TestSuccess] =
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
            "src"
          )

        val actual = read(cListConfig from src).mapError(_.getMessage)

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
            "src"
          )

        val actual = read(cListConfig from src).mapError(_.getMessage)

        val expected = ListConfig(Nil)

        assertM(actual)(equalTo(expected))
      },
      testM("key doesn't exist in list") {
        val src                                              = ConfigSource.fromPropertyTree(
          PropertyTree.Sequence(List(Record(Map()))),
          "src"
        )
        val optional: ConfigDescriptor[Option[List[String]]] = list(string("keyNotExists")).optional
        assertM(read(optional from src).either)(isLeft(anything))
      },
      testM("when empty list") {
        val src                                              = ConfigSource.fromPropertyTree(
          PropertyTree.empty,
          "src"
        )
        val optional: ConfigDescriptor[Option[List[String]]] = list(string("usr")).optional
        assertM(read(optional from src).either)(isRight(isNone))
      },
      testM("list write read") {
        checkM(genListConfig) { p =>
          val actual = ZIO.fromEither(write(cListConfig, p)).flatMap { tree =>
            read(cListConfig from ConfigSource.fromPropertyTree(tree, "tree"))
              .mapError(_.getMessage)
          }
          assertM(actual)(equalTo(p))
        }
      },
      testM("return failure when branch is not defined correctly") {
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
          "src"
        )
        case class AppConfig(branches: Option[List[Branch]])
        case class Branch(pattern: String, tag: Boolean)

        val patternDesc = string("pattern")
        val tagDesc     = boolean("tag")

        val branchConfigDesc =
          (
            patternDesc zip
              tagDesc
          ).to[Branch]

        val appConfigDesc =
          (list("branches")(branchConfigDesc).optional).to[AppConfig]

        assertM(read(appConfigDesc from src).either)(isLeft(anything))
      },
      testM("listOrSingleton on list input") {
        val src =
          ConfigSource.fromPropertyTree(
            Record(
              Map(
                "list" -> PropertyTree.Sequence[String, String](
                  List(PropertyTree.Leaf("x"), PropertyTree.Leaf("y"))
                )
              )
            ),
            "src"
          )

        val config   = listOrSingleton("list")(string)
        val actual   = read(config from src).mapError(_.getMessage)
        val expected = List("x", "y")

        assertM(actual)(equalTo(expected))
      },
      testM("listOrSingleton on singleton input") {
        val src =
          ConfigSource.fromPropertyTree(
            Record(Map("list" -> PropertyTree.Leaf("x"))),
            "src"
          )

        val config   = listOrSingleton("list")(string)
        val actual   = read(config from src).mapError(_.getMessage)
        val expected = List("x")

        assertM(actual)(equalTo(expected))
      }
    )
}

object ListAndOptionalTestUtils {
  final case class OverallConfig(option: Option[Option[Option[Id]]])

  val genOverallConfig: Gen[Random, OverallConfig] =
    Gen.option(genId).map(t => OverallConfig(t.map(t => Option(Option(t)))))

  private def id(path: String) = string(path).to[Id]

  private val cId = id("kId")

  val cOverallConfig: ConfigDescriptor[OverallConfig] =
    cId.optional.optional.optional.to[OverallConfig]

  final case class Opt3Config(a: Id, b: Option[Id], c: Option[Id])
  final case class ListConfig(list: List[Opt3Config])

  val cOpt3Config: ConfigDescriptor[Opt3Config] = (id("a") zip id("b").optional zip id("c").optional).to[Opt3Config]

  val cListConfig: ConfigDescriptor[ListConfig] = list("list")(cOpt3Config).to[ListConfig]

  val genOpt3Config: Gen[Random, Opt3Config] = for {
    a <- genId
    b <- Gen.option(genId)
    c <- Gen.option(genId)
  } yield Opt3Config(a, b, c)

  val genListConfig: Gen[Random with Sized, ListConfig] = Gen.listOf(genOpt3Config).map(ListConfig.apply)
}
