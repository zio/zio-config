package zio.config.yaml

import zio.test._
import zio.test.Assertion._
import zio.config.PropertyTree
import zio.config.ConfigDescriptor

object YamlConfigSpec extends DefaultRunnableSpec {
  val spec = suite("YamlConfig")(
    testM("Read a complex structure") {
      val result = YamlConfigSource.fromString(
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
                    "i" -> PropertyTree.Leaf("1"),
                    "b" -> PropertyTree.Leaf("true"),
                    "s" -> PropertyTree.Leaf("str")
                  )
                ),
                "list" -> PropertyTree.Sequence(
                  List(
                    PropertyTree.Record(Map("i" -> PropertyTree.Leaf("1"))),
                    PropertyTree.Record(Map("b" -> PropertyTree.Leaf("true"))),
                    PropertyTree.Record(Map("s" -> PropertyTree.Leaf("str")))
                  )
                )
              )
            )
          )
        )

      result
        .map(_.getConfigValue(List()))
        .run
        .map(assert(_)(succeeds(equalTo(expected))))
    },
    testM("Read a complex structure into a sealed trait") {
      case class Child(sum: List[Sum])

      sealed trait Sum         extends Product with Serializable
      case class A(a: String)  extends Sum
      case class B(b: Boolean) extends Sum

      val descriptor =
        ConfigDescriptor.nested("sum") {
          ConfigDescriptor.list {
            (ConfigDescriptor.nested("A")(ConfigDescriptor.string("a")(A.apply, A.unapply)) orElseEither
              ConfigDescriptor.nested("B")(ConfigDescriptor.boolean("b")(B.apply, B.unapply)))
              .xmap(_.merge, (_: Sum) match {
                case a: A => Left(a)
                case b: B => Right(b)
              })
          }
        }(Child.apply, Child.unapply)

      val result =
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
