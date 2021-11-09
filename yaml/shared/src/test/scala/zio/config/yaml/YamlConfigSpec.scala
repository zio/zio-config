package zio.config.yaml

import zio.config.{ConfigDescriptor, PropertyTree, PropertyTreePath}
import zio.test.Assertion._
import zio.test._

object YamlConfigSpec extends DefaultRunnableSpec {
  val spec: ZSpec[Environment, Failure] = suite("YamlConfig")(
    testM("Read a complex structure") {
      val result   = YamlConfigSource.fromYamlString(
        """
          |top:
          |  child:
          |    i: 1
          |    b: true
          |    s: "str"
          |  list:
          |  - i: 1
          |  - b: true
          |  - s: "str"
          |""".stripMargin
      )
      val expected =
        PropertyTree.Record(
          Map(
            "top" -> PropertyTree.Record(
              Map(
                "child" -> PropertyTree.Record(
                  Map(
                    "i" -> PropertyTree.Leaf("1", false),
                    "b" -> PropertyTree.Leaf("true", false),
                    "s" -> PropertyTree.Leaf("str", false)
                  )
                ),
                "list"  -> PropertyTree.Sequence(
                  List(
                    PropertyTree.Record(Map("i" -> PropertyTree.Leaf("1", false))),
                    PropertyTree.Record(Map("b" -> PropertyTree.Leaf("true", false))),
                    PropertyTree.Record(Map("s" -> PropertyTree.Leaf("str", false)))
                  )
                )
              )
            )
          )
        )

      assertM(result.runTree(PropertyTreePath(Vector.empty)))(equalTo(expected))
    },
    testM("Read a complex structure into a sealed trait") {
      case class Child(sum: List[Sum])

      sealed trait Sum         extends Product with Serializable
      case class A(a: String)  extends Sum
      case class B(b: Boolean) extends Sum

      val descriptor =
        ConfigDescriptor
          .nested("sum") {
            ConfigDescriptor.list {
              (ConfigDescriptor.nested("A")(ConfigDescriptor.string("a").to[A]) orElseEither
                ConfigDescriptor.nested("B")(ConfigDescriptor.boolean("b").to[B]))
                .transform(
                  _.merge,
                  (_: Sum) match {
                    case a: A => Left(a)
                    case b: B => Right(b)
                  }
                )
            }
          }
          .to[Child]

      val result   =
        YamlConfig.fromString(
          """|sum:
             |- A:
             |    a: "str"
             |- B:
             |    b: false""".stripMargin,
          descriptor
        )
      val expected = Child(List(A("str"), B(false)))

      assertM(result.build.map(_.get).run.useNow)(succeeds(equalTo(expected)))
    }
  )
}
