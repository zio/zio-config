package zio.config.yaml

import zio.ZIO
import zio.config.{Config, PropertyTree, PropertyTreePath, ReadError}
import zio.test.Assertion._
import zio.test.{ZIOSpecDefault, _}

object YamlConfigSpec extends ZIOSpecDefault {
  def spec: Spec[Any, Config.Error] = suite("YamlConfig")(
    test("Read a complex structure") {
      val result = YamlConfigSource.fromYamlString(
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

      assertZIO(result.runTree(PropertyTreePath(Vector.empty)))(equalTo(expected))
    },
    test("Read a complex structure into a sealed trait") {
      case class Child(sum: List[Sum])

      sealed trait Sum         extends Product with Serializable
      case class A(a: String)  extends Sum
      case class B(b: Boolean) extends Sum

      val descriptor =
        Config
          .nested("sum") {
            Config.list {
              (Config.nested("A")(Config.string("a").to[A]) orElseEither
                Config.nested("B")(Config.boolean("b").to[B]))
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
