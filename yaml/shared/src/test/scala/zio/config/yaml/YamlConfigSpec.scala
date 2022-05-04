package zio.config.yaml

import zio.ZIO
import zio.config.{ConfigDescriptor, PropertyTree, PropertyTreePath, ReadError}
import zio.test.Assertion._
import zio.test.{ZIOSpecDefault, _}

object YamlConfigSpec extends ZIOSpecDefault {
  def spec: Spec[Any, ReadError[String]] = suite("YamlConfig")(
    test("Read a complex structure") {
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

      assertZIO(result.runTree(PropertyTreePath(Vector.empty)))(equalTo(expected))
    },
    test("Read a complex structure into a sealed trait") {
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

      assertZIO(ZIO.scoped(result.build.map(_.get).exit))(succeeds(equalTo(expected)))
    }
  )
}
